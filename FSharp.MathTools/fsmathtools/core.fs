#light

module FSharp.MathTools.Core


open Microsoft.FSharp.Math



// Standard mathematical constants
//------------------------------------------------------------------------------------
/// The constant pi = 3.141596...
let PI = System.Math.PI
/// The constant e = 2.718281828
let E = System.Math.E





(*------------------------------------------------------------------------------------

    This section implements the computation of the gamma and logarithm of the gamma
    function. This implementation follows the derivation in
        "An Analysis Of The Lanczos Gamma Approximation", Glendon Ralph Pugh, 2004.
    
    We use the implementation listed on p. 116 which achieves an accuracy of 16
    floating point digits. Improving accuracy is possible see p. 126 in Pugh.

*)
// This section lists the coefficients necessary to compute the gamma function. Pugh, p 116.
let Gamma_n = 10
let Gamma_r = 10.900511
let Gamma_dk = [| 2.48574089138753565546e-5;
                  1.05142378581721974210;
                  -3.45687097222016235469;
                  4.51227709466894823700;
                  -2.98285225323576655721;
                  1.05639711577126713077;
                  -1.95428773191645869583e-1;
                  1.70970543404441224307e-2;
                  -5.71926117404305781283e-4;
                  4.63399473359905636708e-6;
                  -2.71994908488607703910e-9 |]

/// Computes the logarithm of the gamma function for real numbers.
let GammaLn z =    
    /// This method computes the logarithm of the gamma function in the upper half plane.
    let gammaln_p z =
        let mutable s = Gamma_dk.[0]
        for i=1 to Gamma_n do
            s <- s + Gamma_dk.[i] / (z + (float (i-1)))
        (log s) + (log 2.0) + 0.5 * (log (E/PI)) + (z - 0.5) * (log ((z - 0.5 + Gamma_r)/E))

    if z < 0.5 then                                     // TODO how do we choose when to mirror?
        log PI - log (sin (PI*z)) - gammaln_p(1.0-z)
    else
        gammaln_p z

/// Computes the gamma function for real numbers.
let Gamma z =
    /// This method computes the logarithm of the gamma function in the upper half plane.
    let gamma_p z =
        let mutable s = Gamma_dk.[0]
        for i=1 to Gamma_n do
            s <- s + Gamma_dk.[i] / (z + (float (i-1)))
        s * 2.0 * sqrt (E/PI) * ((z - 0.5 + Gamma_r)/E)**(z - 0.5)
        
    if z < 0.5 then                                     // TODO how do we choose when to mirror?
        PI / (sin(PI*z) * gamma_p(1.0-z))
    else
        gamma_p z




(*------------------------------------------------------------------------------------

    This section implements a number of other common mathematical functions (some which
    derive from the functions implemented above).

*)
// This is the largest integer value for which the factorial function doesn't overflow the floating point format.
let Factorial_max = 170
// This is a small precomputed cache of the factorial values.
let Factorial_cache =
    let cache =  [| 0 .. Factorial_max |] |> Array.map (fun a -> float a)
    cache.[0] <- 1.0
    for i in [1 .. Factorial_max] do
        cache.[i] <- cache.[i-1] * (float i)
    cache

/// The factorial functions takes an int x and computes x!. This function will not overflow
/// the floating point format as long as x is at most 170.
let Factorial (x:int) =    
    if x <= Factorial_max then
        Factorial_cache.[x]
    else
        System.Double.PositiveInfinity
        
/// Computes the natural logarithm of the factorial function.
let FactorialLn (x: int) : float =
    if x < 0 then failwith "Log factorial not defined for n < 0"
    if x <= 1 then 0.0 else GammaLn ((float x) + 1.0)

/// Computes the binomial coefficient.
let Binomial (n: int) (h: int) = floor (0.5 + exp ((FactorialLn n) - (FactorialLn h) - (FactorialLn (n-h))))

/// Computes the multinomial coefficient.
let Multinomial (n: int) (ni: int array) = floor (0.5 + exp ((FactorialLn n) - (Array.fold_left (fun acc a -> acc + (FactorialLn a)) 0.0 ni)))

/// Computes the natural logarithm of the beta function.
let BetaLn z w = (GammaLn z) + (GammaLn w) - (GammaLn (z+w))

/// Computes the beta function.
let Beta z w = exp (BetaLn z w)

/// Computes the error function. Note that this implementation has only been verified to have a relative error of around 1e-5.
let rec Erf x =
    // Reference: Abramowitz and Stegun - Handbook of Mathematical Functions, p299.
    if x < 0.0 then
        - (Erf (-x))
    else
        let p = 0.3275911
        let a = [| 0.254829592;
                   -0.284496736;
                   1.421413741;
                   -1.453152027;
                   1.061405429 |]
        let t = 1.0 / (1.0 + p*x)
        1.0 - (exp (-(x*x))) * t * (a.[0] + t*(a.[1] + t*(a.[2] + t*(a.[3] + t*a.[4]))))

/// Computes the complement of the error function. Note that this implementation has only been verified to have a relative error of around 1e-4.
let rec Erfc x =
    // Reference: Abramowitz and Stegun - Handbook of Mathematical Functions, p299.
    if x < 0.0 then
        2.0 - (Erfc (-x))
    else
        let p = 0.3275911
        let a = [| 0.254829592;
                   -0.284496736;
                   1.421413741;
                   -1.453152027;
                   1.061405429 |]
        let t = 1.0 / (1.0 + p*x)
        (exp (-(x*x))) * t * (a.[0] + t*(a.[1] + t*(a.[2] + t*(a.[3] + t*a.[4]))))



/// Computes the cumulative sum of an array.
let CumulativeSum (arr: float array) = Array.scan1_left (fun acc x -> acc + x) arr

/// Computes a histogram with n bins for an array of numbers. The method returns the counts and means for the bins.
let Histogram (X: float []) n =
    if X.Length = 0 then failwith "Nonzero number of datapoints expected."
    let H = Array.create n 0
    // Find the minimal and maximal element in the input.
    let xmin = Array.fold1_left (fun a b -> min a b) X
    let xmax = Array.fold1_left (fun a b -> max a b) X
    // Create an array of means.
    let segs = Array.init (n+1) (fun i -> if i = 0 then xmin
                                          elif i = n then xmax
                                          else xmin + (float i) * (xmax - xmin) / (float n))
    // Count the number of elements in each bin.
    Array.iter (fun a -> let segfind = Array.tryfind_index (fun x -> x > a) segs
                         let segidx = if Option.is_none segfind then n-1 else (Option.get segfind) - 1
                         //printf "%d\n" segidx
                         H.[segidx] <- H.[segidx] + 1
                         ()) X
    H, segs