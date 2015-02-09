module Lafayette.Examples

open System
open System.Threading.Tasks
open Microsoft.FSharp.Reflection
open System.Linq.Expressions
open Orleans

open Lafayette.Monad

type Occupation = Dentist of float
                | Developer of int * List<float>

let askDentist x = GrainFunc.from 1L (Dentist x)

let testGrain = function
    | Dentist x -> grain {
        printfn "Dentist: %.2f (start)" x
        do! grain.Sleep(100)
        printfn "Dentist: %.2f (done)" x
        return int (x * 1.1)
      }
    | Developer (n, xs) -> grain {
        printfn "Developer: %d (start)" n
        let! xs' = Task.WhenAll (Seq.map askDentist xs)
        let n' = float n * Seq.average xs'
        do! grain.Sleep 100
        printfn "Developer: %d (done)" n
        return int n'
      }

let mklist x y z = [x; y; z]
