#light

open System.Diagnostics


/// This method times the execution of a function and returns the number of ticks for execution.
let Time f =
    let sw = new Stopwatch()
    sw.Start()
    f() |> ignore
    sw.ElapsedTicks


// This is a little experiment to test whether the exponentiation operator is faster than squaring.
let x = 324570.345872345978
do
    let rnd = new System.Random()
    let x = rnd.NextDouble()
    let n = 10000000
    let t1 = Time (fun () -> for i=1 to n do
                                let x2 = x*x
                                ()
                 )
    printf "x*x took %d ticks for %d iterations.\n" t1 n
    let t2 = Time (fun () -> for i=1 to n do
                                let x2 = x**2.0
                                ()
                 )
    printf "x**2.0 took %d ticks for %d iterations.\n" t2 n