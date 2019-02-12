#light

#r "..\fsmathtools.dll"

open System.IO
open Microsoft.FSharp.Math

// Correct vector norm implementation.
let norm (x: vector) = sqrt(x.Transpose * x)

/// This is the default relative error which we want to tolerate
/// when testing functions against a file with known function values.
let DoubleTolerance = 1.0e-11

/// This is the default relative error which we want to tolerate
/// when testing functions with single precision accuracy.
let SingleTolerance = 3.0e-4


/// This method compares the results of a function taking only one parameters
/// with values retrieved from a file (name of the function + ".txt" from the
/// RegressionTests directory. The function returns the largest relative error
/// and the first inputs that caused it.
let Test1ParamFunction (name:string) tolerance testfun =
    let mutable reterr = 0.0                // Saves the current largest relative error.
    let mutable retval = 0.0                // Saves the inputs for which this error occured.
    printf "Regression testing %s function.\n" name
    let dir = System.Environment.CurrentDirectory ^ @"\"
    let sr = new StreamReader(dir ^ name ^ ".txt")
    while not sr.EndOfStream do
        let line = sr.ReadLine()
        let words = String.split [' '] line
        let fin = Float.of_string (List.hd words)                       // The input which we want to test.
        let fout = Float.of_string (List.hd (List.tl words))            // The "correct" value.
        let feval = testfun fin                                         // The value computes by our library.
        let relerr = (abs ((feval - fout) / fout))
        assert(relerr < tolerance)
        if relerr > reterr then
            reterr <- relerr
            retval <- fin
    sr.Close()
    printf "   OK (worst rel. err: %e with %A)\n" reterr retval
    (reterr, retval)


/// This method compares the results of a function taking only two parameters
/// with values retrieved from a file (name of the function + ".txt" from the
/// RegressionTests directory. The function returns the largest relative error
/// and the first inputs that caused it.
let Test2ParamFunction (name:string) tolerance testfun =
    let mutable reterr = 0.0                // Saves the current largest relative error.
    let mutable retval = (0.0,0.0)          // Saves the inputs for which this error occured.
    printf "Regression testing %s function.\n" name
    let dir = System.Environment.CurrentDirectory ^ @"\"
    let sr = new StreamReader(dir ^ name ^ ".txt")
    while not sr.EndOfStream do
        let line = sr.ReadLine()
        let words = String.split [' '] line
        let fin1 = Float.of_string (List.hd words)                       // The inputs which we want to test.
        let fin2 = Float.of_string (List.hd (List.tl words))
        let fout = Float.of_string (List.hd (List.tl (List.tl words)))   // The "correct" value.
        let feval = testfun fin1 fin2                                    // The value computes by our library.
        let relerr = (abs ((feval - fout) / fout))
        assert(relerr < tolerance)
        if relerr > reterr then
            reterr <- relerr
            retval <- (fin1,fin2)
    sr.Close()
    printf "   OK (worst rel. err: %e with %A)\n" reterr retval
    (reterr, retval)










printf @"
----------------------------------------------------------------------------
    Regression testing FSharp.MathTools.Core with tolerance %e.
----------------------------------------------------------------------------
" DoubleTolerance

(* Matlab code for generating regression values:
clear;
n = 1000;
v(1:n,1) = 100.0 * rand(n,1) - 50.0;
v(1:n,2) = gamma(v(1:n,1));

save 'gamma.txt' v -ASCII -double
*)
Test1ParamFunction "Gamma" DoubleTolerance FSharp.MathTools.Core.Gamma


(* Matlab code for generating regression values:
clear;
n = 1000;
v(1:n,1) = 100.0 * rand(n,1);
v(1:n,2) = gammaln(v(1:n,1));

save 'gammaln.txt' v -ASCII -double
*)
Test1ParamFunction "GammaLn" DoubleTolerance FSharp.MathTools.Core.GammaLn


(* Matlab code for generating regression values:
clear;
v(:,1) = 1:170;
v(:,2) = factorial(v(:,1));

save 'factorial.txt' v -ASCII -double
*)
Test1ParamFunction "Factorial" DoubleTolerance (fun f -> FSharp.MathTools.Core.Factorial (int f))


(* Matlab code for generating regression values:
clear;
n = 1000;
v(:,1) = 100.0 * rand(n,1);
v(:,2) = 100.0 * rand(n,1);
v(:,3) = beta(v(:,1), v(:,2));

save 'beta.txt' v -ASCII -double
*)
Test2ParamFunction "Beta" DoubleTolerance FSharp.MathTools.Core.Beta


(* Matlab code for generating regression values:
clear;
n = 1000;
v(:,1) = 100.0 * rand(n,1);
v(:,2) = 100.0 * rand(n,1);
v(:,3) = betaln(v(:,1), v(:,2));

save 'betaln.txt' v -ASCII -double
*)
Test2ParamFunction "BetaLn" DoubleTolerance FSharp.MathTools.Core.BetaLn


(* Matlab code for generating regression values:
clear;
n = 2000;
v(:,1) = 5*(rand(n,1) - 0.5);
v(:,2) = erf(v(:,1));

save 'erf.txt' v -ASCII -double
*)
Test1ParamFunction "erf" SingleTolerance FSharp.MathTools.Core.Erf


(* Matlab code for generating regression values:
clear;
n = 2000;
v(:,1) = 5*(rand(n,1) - 0.5);
v(:,2) = erfc(v(:,1));

save 'erfc.txt' v -ASCII -double
*)
Test1ParamFunction "erfc" SingleTolerance FSharp.MathTools.Core.Erfc










printf @"
----------------------------------------------------------------------------
    Regression testing FSharp.MathTools.Optimization.
----------------------------------------------------------------------------
"

/// This is a small collection of test functions. When calling a constructor, it returns the function, the gradient,
/// the Hessian, a starting point and a minimum of the function. Be aware that this is just one possible minimum for
/// some of our functions.
module TestFunctions =
    module OneDimensional =
        let Quadratic () = ( (fun x -> x.[0]*x.[0]),
                             (fun x -> Vector.of_list [2.0 * x.[0]]),
                             (fun x -> Matrix.of_list [[2.0]]),
                             Vector.of_list [2.0],
                             Vector.of_list [0.0])
    module TwoDimensional =
        let WellConditioned () =  ( (fun (x:vector) -> x.[0]*x.[0] + 5.0 * x.[1]*x.[1] + x.[0] - 5.0*x.[1]),
                                    (fun (x:vector) -> Vector.of_list [2.0*x.[0] + 1.0; 10.0*x.[1] - 5.0]),
                                    (fun (x:vector) -> Matrix.of_list [[2.0; 0.0]; [0.0; 10.0]]),
                                    Vector.of_list [-1.2; 1.0],
                                    Vector.of_list [-0.5; 0.5])
        let IllConditioned () =  ( (fun (x:vector) -> 1e10*x.[0]*x.[0] + 5.0 * x.[1]*x.[1] + x.[0] - 5.0*x.[1]),
                                   (fun (x:vector) -> Vector.of_list [2.0e10*x.[0] + 1.0; 10.0*x.[1] - 5.0]),
                                   (fun (x:vector) -> Matrix.of_list [[2.0e10; 0.0]; [0.0; 10.0]]),
                                   Vector.of_list [-1.2; 1.0],
                                   Vector.of_list [-0.5e-10; 0.5])
        let McCormic () = ( (fun (x:vector) -> sin(x.[0] + x.[1]) + (x.[0] - x.[1]) ** 2.0 - 1.5 * x.[0] + 2.5 * x.[1] + 1.0),
                            (fun (x:vector) -> Vector.of_list [cos(x.[0] + x.[1]) + 2.0 * (x.[0] - x.[1]) - 1.5; cos(x.[0] + x.[1]) - 2.0 * (x.[0] - x.[1]) + 2.5]),
                            (fun (x:vector) -> Matrix.of_list [[2.0; 5.0]; [5.0; 200.0]]),
                            Vector.of_list [-1.5; 4.0],
                            Vector.of_list [-0.54719; -1.54719])
        let Rosenbrock () = ( (fun (x:vector) -> let z = x.[1] - x.[0]**2.0 in 100.0*z**2.0 + (1.0-x.[0])**2.0),
                              (fun (x:vector) -> let z = x.[1] - x.[0]**2.0 in Vector.of_list [-400.0*x.[0]*z + 2.0*x.[0] - 2.0; 200.0*z]),
                              (fun (x:vector) -> let z = x.[1] - x.[0]**2.0 in Matrix.of_list [[1200.0*x.[0]**2.0 - 400.0*x.[1]+2.0; -400.0*x.[0]]; [-400.0*x.[0]; 200.0]]),
                              Vector.of_list [0.0; 0.0],
                              Vector.of_list [1.0; 1.0])
                            


// Backtrack line search.
do
    printf "Regression testing BackTrackLineSearch.\n"
    let bls = new FSharp.MathTools.Optimization.BackTrackLineSearch()
    let f,g,_,x0,xt = TestFunctions.OneDimensional.Quadratic ()
    let (alpha, x) = (bls :> FSharp.MathTools.Optimization.ILineSearch).FindStep f g (new FSharp.MathTools.Optimization.Point(x0,f,g)) (-(g x0))
    if x.f < (f x0) then printf "   OK\n" else printf "   ERROR: Backtracking line search failed.\n"
    
// Wolfe line search.
do
    printf "Regression testing WolfeLineSearch.\n"
    let wls = new FSharp.MathTools.Optimization.WolfeLineSearch()
    let f,g,_,x0,xt = TestFunctions.OneDimensional.Quadratic ()
    let (alpha, x) = (wls :> FSharp.MathTools.Optimization.ILineSearch).FindStep f g (new FSharp.MathTools.Optimization.Point(x0,f,g)) (-(g x0))
    if x.f < (f x0) then printf "   OK\n" else printf "   ERROR: Wolfe line search failed.\n"

// Gradient descent. 
do
    // This should work.
    printf "Regression testing gradient descent on well conditioned function.\n"
    let gd = new FSharp.MathTools.Optimization.GradientDescent()
    let f,g,_,x0,xt = TestFunctions.TwoDimensional.WellConditioned()
    //DEBUG gd.Callback <- (fun i x -> printf "%d: %A, %A\n" i x.p x.g)
    let x, i = gd.Run f g x0
    if norm(x.g) <= gd.Stop then printf "   OK\n" else printf "   ERROR: Gradient descent failed on well conditioned function.\n"
do
    // This should not work.
    printf "Regression testing gradient descent on Rosenbrock function.\n"
    let gd = new FSharp.MathTools.Optimization.GradientDescent()
    let f,g,_,x0,xt = TestFunctions.TwoDimensional.Rosenbrock()
    //DEBUG gd.Callback <- (fun i x -> printf "%d: %A, %A\n" i x.p x.g)
    let x, i = gd.Run f g x0
    if norm(x.g) > gd.Stop && i = gd.MaxIter then printf "   OK\n" else printf "   ERROR: Gradient descent shouldn't have worked on Rosenbrock.\n"

// Conjugate gradient; all the test functions should work.
do
    printf "Regression testing conjugate gradient on well conditioned function.\n"
    let cg = new FSharp.MathTools.Optimization.ConjugateGradient()
    let f,g,_,x0,xt = TestFunctions.TwoDimensional.WellConditioned()
    //DEBUG cg.Callback <- (fun i x -> printf "%d: %A, %f\n" i x.p (norm x.g))
    let x, i = cg.Run f g x0
    if norm(x.g) <= cg.Stop then printf "   OK\n" else printf "   ERROR: Conjugate gradient failed on well conditioned function.\n"
do
    printf "Regression testing conjugate gradient on Rosenbrock function.\n"
    let cg = new FSharp.MathTools.Optimization.ConjugateGradient()
    let f,g,_,x0,xt = TestFunctions.TwoDimensional.Rosenbrock()
    //DEBUG cg.Callback <- (fun i x -> printf "%d: %A, %f\n" i x.p (norm x.g))
    let x, i = cg.Run f g x0
    if norm(x.g) <= cg.Stop then printf "   OK\n" else printf "   ERROR: Conjugate gradient failed on Rosenbrock function.\n"
do
    printf "Regression testing conjugate gradient on McCormic function.\n"
    let cg = new FSharp.MathTools.Optimization.ConjugateGradient()
    let f,g,_,x0,xt = TestFunctions.TwoDimensional.McCormic()
    //DEBUG cg.Callback <- (fun i x -> printf "%d: %A, %f\n" i x.p (norm x.g))
    let x, i = cg.Run f g x0
    if norm(x.g) <= cg.Stop then printf "   OK\n" else printf "   ERROR: Conjugate gradient failed on McCormic function.\n"
do
    // For the ill conditioned function, we check whether our method is close to the analytic solution since the gradient will not be
    // smaller than 1e-6.
    printf "Regression testing conjugate gradient on ill conditioned function.\n"
    let cg = new FSharp.MathTools.Optimization.ConjugateGradient()
    let f,g,_,x0,xt = TestFunctions.TwoDimensional.IllConditioned()
    //DEBUG cg.Callback <- (fun i x -> printf "%d: %A, %f, %f\n" i x.p (norm x.g) x.f)
    let x, i = cg.Run f g x0
    if norm(x.p - xt) <= 1e-6 then printf "   OK\n" else printf "   ERROR: Conjugate gradient failed on ill conditioned function.\n"
    









printf "\n\n\nPress any key to exit the regression test procedure ...\n\n"
System.Console.ReadLine() |> ignore