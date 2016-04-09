open Microsoft.Z3
open NonNF

(* 
let ctx = new Context()

let x = ctx.MkTrue()
let y = ctx.MkFalse()

let z1 = ctx.MkConst("z1", ctx.MkBoolSort())
let z2 = ctx.MkConst("z2", ctx.MkBoolSort())

let expr1 = ctx.MkEq(x, y)

let expr2 = ctx.MkAnd([ x ; y ] |> List.toArray)

let expr3 = ctx.MkEq(z1, x)

let solver = ctx.MkSolver()
*)

let xs = [ "a"; "a"; "a"; "b"; "abc"; "bca" ]
let ys = []

[<EntryPoint>]
let main argv = 
    (*solver.Assert(expr3)
    if solver.Check() = Status.SATISFIABLE then printfn "SAT\n" else printfn "UNSAT\n"
    let m = solver.Model
    for d in m.Decls do
        System.Console.WriteLine("{0} -> {1}", d.Name, m.ConstInterp(d))
        *)
    printfn "%A" (checkPref "ab" "ab")
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
