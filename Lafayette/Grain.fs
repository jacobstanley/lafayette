namespace Lafayette

open System
open System.Threading.Tasks
open System.Linq.Expressions
open System.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Reflection
open Orleans

////////////////////////////////////////////////////////////////////////

module Monad =
    type GrainMonad () =
        member o.Return (x: 'a) =
            Task.FromResult x
        member o.Bind (m: Task<'a>, f: 'a -> Task<'b>) =
            let f' (t: Task<'a>) = f(t.Result)
            (m.ContinueWith f').Unwrap ()

        member o.Sleep (ms: int) =
            let f (_: Task) = Task.FromResult ()
            ((Task.Delay ms).ContinueWith f).Unwrap ()

    let grain = GrainMonad ()

////////////////////////////////////////////////////////////////////////

module Util =
    let isFunc a b (m: MethodInfo) =
        m.IsStatic && m.IsPublic &&
        m.ReturnType = b &&
        let ps = m.GetParameters ()
        in ps.Length = 1 && ps.[0].ParameterType = a

    let mkFunc<'a, 'b> (m: MethodInfo) =
        if isFunc typeof<'a> typeof<'b> m
        then let x = Expression.Parameter typeof<'a>
             let lam = Expression.Lambda<Converter<'a, 'b>> (Expression.Call(m, x), x)
             Some (FSharpFunc.FromConverter (lam.Compile ()))
        else None

    let findFunc<'a, 'b> : ('a -> 'b) =
        let asms = AppDomain.CurrentDomain.GetAssemblies ()

        let fs = asms |> Seq.collect (fun x -> x.GetExportedTypes ())
                      |> Seq.filter FSharpType.IsModule
                      |> Seq.collect (fun x -> x.GetMethods ())
                      |> Seq.choose (fun x -> Option.map (fun f -> (f, x)) (mkFunc<'a, 'b> x))

        for (f, m) in fs do
          printfn "%s: %s -> %s" m.Name typeof<'a>.Name typeof<'b>.Name

        let (f, m) = Seq.head fs
        f

////////////////////////////////////////////////////////////////////////

type GrainFunc<'a,'b> () =
    inherit Grain ()
    let invoke = Util.findFunc
    interface IGrainFunc<'a,'b> with
        member o.Invoke (req: 'a) = invoke req
