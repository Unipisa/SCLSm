#light

module FSharp.MathTools.Compatibility.Matlab

open Microsoft.FSharp.Math

open FSharp.MathTools


// Standard mathematical constants
//------------------------------------------------------------------------------------
/// The constant pi = 3.141596...
let pi = System.Math.PI
/// Float infinity.
let inf = System.Double.PositiveInfinity
/// Float NaN.
let NaN = System.Double.NaN
/// Returns the logarithm for x in base 2.
let log2 x = System.Math.Log(x, 2.0)
/// Returns the logarithm for x in base 10.
let log10 x = System.Math.Log10(x)


// Functions for statistics and probability
//------------------------------------------------------------------------------------
/// Generates a uniformly distributed random floating point number in the interval [0,1].
let rand () = Stat.GetSampleGenerator().NextFloat()
/// Generates a normal distributed random floating point number.
let randn () =
    let v1 = ref(2.0 * rand() - 1.0)
    let v2 = ref(2.0 * rand() - 1.0)
    let r = ref (!v1 * !v1 + !v2 * !v2)
    while (!r >= 1.0 || !r = 0.0) do
        v1 := 2.0 * rand() - 1.0
        v2 := 2.0 * rand() - 1.0
        r := !v1 * !v1 + !v2 * !v2
    let fac = sqrt(-2.0*log(!r)/(!r))
    !v1 / fac
/// Generates a random permutation of the numbers 0..n-1 using a Knuth shuffle.
let randperm (n: int) =
    let id = [|0..n-1|]
    for i=0 to n-2 do
        let l = int (floor (float (n-i) * rand())) + i
        let t = id.[l]
        id.[l] <- id.[i]
        id.[i] <- t
    id
/// Returns the value for the density function of the normal probability distribution with
/// mean mu and standard deviation tau.
let normpdf x mu tau = exp(-0.5*(x-mu)*(x-mu)/(tau*tau)) / sqrt(2.0*Core.PI*tau*tau)
/// Returns a random number from a normal distribution with mean my and standard deviation sdev.
let normrand mu sdev = sdev*randn() + mu
/// Computes the cumulative sum of the elements in an array.
let cumsum (arr: float array) = Array.scan1_left (fun acc x -> acc + x) arr


// Matrix and vector helper functions
//------------------------------------------------------------------------------------
/// Generates a rowvector of n linearly spaced points between and including a and b.
let linspace a b n = RowVector.init n (fun i -> a + (float i) * ((b-a) / ((float n) - 1.0)))

/// Make a matrix which replicate the matrix A, m times along the row dimension and n times along the column dimension.
let repmat A m n = let (mA,nA) = Matrix.dims A in Matrix.init (m * mA) (n * nA) (fun i j -> A.[i % mA, j % nA])


// Utility helper functions
//------------------------------------------------------------------------------------
/// This object saves the timer used for calculating tic/toc timing information.
let mutable tictocTimer = null
/// Saves the current time.
let tic () =
    tictocTimer <- new System.Diagnostics.Stopwatch()
    tictocTimer.Start()
/// Returns the time in seconds since calling toc.
let toc () =
    if tictocTimer = null then failwith "Use tic to start timer before calling toc."
    else (float tictocTimer.ElapsedMilliseconds) / 1000.0