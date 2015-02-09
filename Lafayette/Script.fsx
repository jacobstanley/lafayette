#r "mscorlib"
#r "System.Core.dll"
#r "C:/Microsoft Codename Orleans SDK v0.9/SDK/Binaries/OrleansClient/Orleans.dll"

open System
open System.Security.Cryptography
open System.Text

let getFullName (t: Type) =
    match t with
    | null                      -> raise (ArgumentNullException "t")
    | _ when t.IsNested         -> t.Namespace + "." + t.DeclaringType.Name + "." + t.Name
    | _ when t.FullName <> null -> t.FullName
    | _                         -> t.Namespace + "." + t.Name

let calculateIdHash (text: String) =
    use sha = new SHA256CryptoServiceProvider ()
    let xs = Encoding.Unicode.GetBytes text |> sha.ComputeHash
    let mutable hash = 0

    printfn "%s" text
    for i in 0 .. 4 .. xs.Length-1 do
        printfn "%d -> %d" i (i+3)
        let n = (int xs.[i]   <<< 24)
            ||| (int xs.[i+1] <<< 16)
            ||| (int xs.[i+2] <<< 8)
            ||| (int xs.[i+3])
        hash <- hash ^^^ n
    hash

let interfaceId: Type -> int = calculateIdHash << getFullName

//let foo = interfaceId typeof<IFoo<_>>
let bar = calculateIdHash "Lafayette.Interfaces.IMailbox"
