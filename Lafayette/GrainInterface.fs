namespace Lafayette

open System
open System.Threading.Tasks
open System.Runtime.Serialization
open Orleans
open Orleans.CodeGeneration
open Orleans.Runtime

////////////////////////////////////////////////////////////////////////

module IGrainFunc =
    [<Literal>]
    let id = 1
    [<Literal>]
    let name = "Lafayette.IGrainFunc<a,b>"
    [<Literal>]
    let invokeId = 1
    [<Literal>]
    let invokeName = "Invoke"

[<TypeCodeOverride(IGrainFunc.id)>]
type IGrainFunc<'a,'b> =
    inherit IGrain
    abstract member Invoke : 'a -> Task<'b>

////////////////////////////////////////////////////////////////////////

[<MethodInvoker(IGrainFunc.name, IGrainFunc.id)>]
type GrainFuncMethodInvoker<'a,'b> () =
    interface IGrainMethodInvoker with
        member o.InterfaceId = IGrainFunc.id

        member o.Invoke (grain, iid, mid, args) =
            try
                if grain = null
                then raise (new ArgumentNullException "grain")

                if iid <> IGrainFunc.id
                then raise (new InvalidCastException ("interfaceId=" + string iid))

                if mid <> IGrainFunc.invokeId
                then raise (new NotImplementedException ("interfaceId=" + string iid + ",methodId=" + string mid))

                let func = downcast grain : IGrainFunc<'a,'b>
                func.Invoke(downcast args.[0])
                    .ContinueWith(fun (t: Task<'b>) -> if t.Status = TaskStatus.Faulted
                                                       then raise t.Exception
                                                       else upcast t.Result : obj)
            with ex ->
                let t = TaskCompletionSource<obj> ()
                t.SetException ex
                t.Task

    static member GetMethodName iid mid =
        if iid <> IGrainFunc.id
        then raise (new InvalidCastException ("interfaceId=" + string iid))

        if mid <> IGrainFunc.invokeId
        then raise (new NotImplementedException ("interfaceId=" + string iid + ",methodId=" + string mid))

        IGrainFunc.invokeName

////////////////////////////////////////////////////////////////////////

[<Serializable>]
[<GrainReference(IGrainFunc.name)>]
type GrainFuncReference<'a,'b> =
    inherit GrainReference
    interface IAddressable

    new (reference)     = { inherit GrainReference (reference)     }
    new (info, context) = { inherit GrainReference (info, context) }

    static member Cast grainRef : IGrainFunc<'a,'b> =
        let target = typeof<IGrainFunc<'a,'b>>
        let mkRef  = Func<GrainReference,IAddressable> (fun x -> upcast GrainFuncReference<'a,'b>(x))
        downcast GrainReference.CastInternal(target, mkRef, grainRef, IGrainFunc.id)

    [<CopierMethod>]
    static member _Copier (original: obj) : obj =
        let input  = downcast original : GrainFuncReference<'a,'b>
        let output = downcast GrainReference.CopyGrainReference input : GrainFuncReference<'a,'b>
        upcast output

    [<SerializerMethod>]
    static member _Serializer (original: obj) stream expected =
        let input  = downcast original : GrainFuncReference<'a,'b>
        GrainReference.SerializeGrainReference (input, stream, expected)

    [<DeserializerMethod>]
    static member _Deserializer expected stream =
        let input = downcast GrainReference.DeserializeGrainReference (expected, stream) : GrainReference
        GrainFuncReference<'a,'b>.Cast input

    override o.InterfaceId    = IGrainFunc.id
    override o.InterfaceName  = IGrainFunc.name

    override o.IsCompatible  iid        = IGrainFunc.id = iid
    override o.GetMethodName (iid, mid) = GrainFuncMethodInvoker<'a,'b>.GetMethodName iid mid

    interface IGrainFunc<'a,'b> with
        member o.Invoke x = base.InvokeMethodAsync<'b> (IGrainFunc.invokeId, [| x |])

////////////////////////////////////////////////////////////////////////

module GrainFunc =
    let private cast<'a,'b> grainRef =
        let grain = GrainFuncReference<'a,'b>.Cast grainRef
        grain.Invoke

    let from<'a,'b> (primaryKey: int64) =
        cast<'a,'b> <| GrainFactoryBase.MakeGrainReferenceInternal
                       (typeof<IGrainFunc<'a,'b>>, IGrainFunc.id, primaryKey, null)

    let fromGuid<'a,'b> (primaryKey: Guid) =
        cast<'a,'b> <| GrainFactoryBase.MakeGrainReferenceInternal
                       (typeof<IGrainFunc<'a,'b>>, IGrainFunc.id, primaryKey, null)
