#light

module FSharp.MathTools.Stat

open FSharp.MathTools
open Microsoft.FSharp.Math

// Probability and statistics functions
//------------------------------------------------------------------------------------
/// Interface that every uniform random number generator must implement.
type IRandom = interface
    abstract NextInt : unit -> int
    abstract NextFloat : unit -> float
end

/// A standard implementation of a uniform random source using System.Random()
type RandBasic =
    val mutable rnd : System.Random
    /// Constructs the default random number generator with seed 17.
    new() = { rnd = new System.Random(17) }
    /// If n is negative, the random number generator seed is based on system time, if it is zero or positive it will
    /// use n as the seed.
    new(n) as this = { rnd = new System.Random() }
                     then
                        if n >= 0 then this.rnd <- new System.Random(n)
    interface IRandom with
        member x.NextInt() = x.rnd.Next()
        member x.NextFloat() =x.rnd.NextDouble()
    end


/// The uniform random source used for sampling functions.
let mutable rndgen = new RandBasic() :> IRandom
/// Sets the random number generator used for sampling.
let SetSampleGenerator rg = rndgen <- rg
/// Returns the random number generator used for sampling.
let GetSampleGenerator () = rndgen


/// Interface which every probability distribution must implement.
type IDistribution<'a, 'b> =
    abstract Mean : 'a
    abstract StandardDeviation : 'a
    abstract Variance : 'a
    abstract CoVariance : 'b
    abstract Sample : unit -> 'a
    abstract PDF : 'a -> float
    abstract CDF : 'a -> float


/// This module contains various probability distributions including methods to sample from them, evaluate pdf and cdf functions, ...
module Distributions =
    // Uniform distribution helper functions.
    let uniformCheckParam min max = if System.Double.IsNaN(min) || System.Double.IsNaN(max) || min > max then failwith "Uniform distribution should be parametrized by min < max in [-inf,inf]."
    
    /// Uniform distribution.
    type Uniform =
        /// Computes the mean.
        static member Mean min max =
            uniformCheckParam min max
            min + (max - min) / 2.0
        /// Computes the variance.
        static member Variance min max =
            uniformCheckParam min max
            1.0/3.0 * (max*max + max * min + min*min)
        /// Computes the standard deviation.
        static member StandardDeviation min max =
            uniformCheckParam min max
            sqrt (1.0/3.0 * (max*max + max * min + min*min))
        /// Produces a random sample using the current random number generator (from GetSampleGenerator()).
        static member Sample min max =
            uniformCheckParam min max
            rndgen.NextFloat() * (max - min) + min
        /// Computes the probability density function.
        static member PDF min max x =
            uniformCheckParam min max
            if x <= max && x >= min then 1.0 / (max - min) else 0.0
        /// Computes the cumulative distribution function.
        static member CDF min max x =
            uniformCheckParam min max
            if x < min then 0.0
            elif x < max then (x - min) / (max - min)
            else 1.0
        /// Returns the support of the exponential distribution: [0, Positive Infinity).
        static member Support min max =
            uniformCheckParam min max
            (System.Double.NegativeInfinity, System.Double.PositiveInfinity)


    // Bernoulli distribution helper functions.
    let bernCheckParam p = if p < 0.0 || p > 1.0 then failwith "Bernoulli distribution should be parametrized by p in [0.0, 1.0]."
    
    /// Bernoulli distribution.
    type Bernoulli =
        /// Computes the mean.
        static member Mean p =
            bernCheckParam p
            p
        /// Computes the variance.
        static member Variance p =
            bernCheckParam p
            p * (1.0 - p)
        /// Computes the standard deviation.
        static member StandardDeviation p =
            bernCheckParam p
            sqrt (p * (1.0 - p))
        /// Produces a random sample using the current random number generator (from GetSampleGenerator()).
        static member Sample p = 
            bernCheckParam p
            if rndgen.NextFloat() < p then 0.0 else 1.0
        /// Computes the probability density function.
        static member PDF p x =
            bernCheckParam p
            match x with
            | 0.0 -> p
            | 1.0 -> 1.0 - p
            | _ -> 0.0
        /// Computes the cumulative distribution function.
        static member CDF p x =
            bernCheckParam p
            if x < 0.0 then 0.0
            elif x < 1.0 then p
            else 1.0
        /// Returns the support of the exponential distribution: [0, Positive Infinity).
        static member Support p =
            bernCheckParam p
            [0.0; 1.0]



    // Multinomial distribution helper functions.
    let multiCheckParam (p: vector) =
        if not (Vector.fold (fun acc f -> acc && (f > 0.0)) true p) then
            failwith "Multinomial distribution should be parametrized with p_i in [0.0, 1.0] and all entries summing to one."
    
    /// Multinomial distribution.
    type Multinomial =
        /// Computes the mean.
        static member Mean (p:vector) (n:int) =
            multiCheckParam p
            (float n) $* p
        /// Computes the variance.
        static member Variance (p:vector) (n:int)  =
            multiCheckParam p
            (float n) $* (Vector.cptMul p (Vector.map (fun x -> 1.0 - x) p))
        /// Computes the standard deviation.
        static member StandardDeviation (p:vector) (n:int) =
            multiCheckParam p
            Vector.map (fun x -> sqrt x) ((float n) $* (Vector.cptMul p (Vector.map (fun x -> 1.0 - x) p)))
        /// Produces a random sample using the current random number generator (from GetSampleGenerator()).
        static member Sample (p:vector) (n:int) =
            multiCheckParam p
            let x = Vector.Generic.create (p.Length) 0
            let cs = Vector.of_array (Core.CumulativeSum (Vector.to_array p))
            for i=0 to n-1 do
                let r = rndgen.NextFloat()
                let t = int (Vector.sum (Vector.map (fun x -> if x < r then 1.0 else 0.0) cs))
                printf "%A: %d\n" cs t
                x.[t] <- x.[t] + 1
            x
        /// Computes the probability density function.
        static member PDF (p:vector) (n:int) (x:Vector<int>) =
            multiCheckParam p
            (Core.Multinomial n (Vector.Generic.to_array x)) * (Vector.foldi ( fun i pi acc -> acc * (pi ** (float x.[i])) ) 1.0 p)


    
    // Normal distribution helper functions.
    let normalCheckParam mu tau = if System.Double.IsNaN(mu) || tau < 0.0 then failwith "Normal distribution should be parametrized by tau > 0.0."
    
    /// Normal distribution.
    type Normal =
        /// Computes the mean.
        static member Mean mu tau =
            normalCheckParam mu tau
            mu
        /// Computes the variance.
        static member Variance mu tau =
            normalCheckParam mu tau
            tau*tau
        /// Computes the standard deviation.
        static member StandardDeviation mu tau =
            normalCheckParam mu tau
            tau
        /// Produces a random sample using the current random number generator (from GetSampleGenerator()).
        static member Sample mu tau =
            normalCheckParam mu tau
            let mutable v1 = 2.0 * rndgen.NextFloat() - 1.0
            let mutable v2 = 2.0 * rndgen.NextFloat() - 1.0
            let mutable r = v1 * v1 + v2 * v2
            while (r >= 1.0 || r = 0.0) do
                v1 <- 2.0 * rndgen.NextFloat() - 1.0
                v2 <- 2.0 * rndgen.NextFloat() - 1.0
                r <- v1 * v1 + v2 * v2
            let fac = sqrt(-2.0*(log r)/r)
            (tau * v1 * fac + mu)
        /// Computes the probability density function.
        static member PDF mu tau x =
            normalCheckParam mu tau
            (exp (-0.5 * (x-mu)*(x-mu) / (tau*tau))) / (sqrt (2.0 * Core.PI))
        /// Computes the cumulative distribution function.
        static member CDF mu tau x =
            normalCheckParam mu tau
            0.5 * (1.0 + Core.Erf((x - mu)/(tau*(sqrt 2.0))))
        /// Returns the support of the exponential distribution: [0, Positive Infinity).
        static member Support mu tau =
            normalCheckParam mu tau
            (System.Double.NegativeInfinity, System.Double.PositiveInfinity)


    // Exponential distribution helper functions.
    let expCheckParam lambda = if lambda <= 0.0 then failwith "Exponential distribution should be parametrized by lambda > 0.0."
    
    /// Exponential distribution.
    type Exponential =
        /// Computes the mean.
        static member Mean lambda =
            expCheckParam lambda
            1.0 / lambda
        /// Computes the variance.
        static member Variance lambda =
            expCheckParam lambda
            1.0 / (lambda * lambda)
        /// Computes the standard deviation.
        static member StandardDeviation lambda =
            expCheckParam lambda
            1.0 / lambda
        /// Produces a random sample using the current random number generator (from GetSampleGenerator()).
        static member Sample lambda = 
            expCheckParam lambda
            let mutable r = rndgen.NextFloat()
            while (r = 0.0) do
                r <- rndgen.NextFloat()
            done;
            (- log r)/lambda
        /// Computes the probability density function.
        static member PDF lambda x = 
            expCheckParam lambda
            if x >= 0.0 then
                - lambda * exp(-lambda * x)
            else 0.0
        /// Computes the cumulative distribution function.
        static member CDF lambda x =
            expCheckParam lambda
            if x < 0.0 then 0.0
            else 1.0 - exp(-lambda * x)
        /// Returns the support of the exponential distribution: [0, Positive Infinity).
        static member Support lambda =
            expCheckParam lambda
            (0.0, System.Double.PositiveInfinity)

    // Gamma distribution helper functions.
    let gammaCheckParam alpha beta = if alpha <= 0.0 || beta <= 0.0 then failwith "Gamma distribution should be parametrized by alpha > 0.0, beta > 0.0."
    
    /// Gamma distribution
    /// Sampling implementation based on:
    ///     "A Simple Method for Generating Gamma Variables" - Marsaglia & Tsang
    ///     ACM Transactions on Mathematical Software, Vol. 26, No. 3, September 2000, Pages 363–372.
    type Gamma =
        static member Mean alpha beta =
            gammaCheckParam alpha beta
            alpha / beta
        static member Variance alpha beta =
            gammaCheckParam alpha beta
            alpha / (beta * beta)
        static member StandardDeviation alpha beta =
            gammaCheckParam alpha beta
            sqrt (alpha / (beta * beta))
        static member Sample alpha beta = 
            gammaCheckParam alpha beta
            let mutable a = alpha
            // Fix when alpha is less than one.
            let alphafix =
                if alpha < 1.0 then
                    a <- alpha + 1.0
                    (rndgen.NextFloat() ** (1.0 / alpha))
                else
                    1.0
            let d = a - 1.0 / 3.0
            let c = 1.0 / sqrt(9.0 * d)
            let rec gamma_sample () =
                let mutable x = Normal.Sample 0.0 1.0
                let mutable v = 1.0 + c * x
                while v <= 0.0 do
                    x <- Normal.Sample 0.0 1.0
                    v <- 1.0 + c * x
                v <- v * v * v
                let u = rndgen.NextFloat()
                x <- x * x
                if u < 1.0 - 0.0331 * x * x then
                    d * v
                elif (log u) < 0.5 * x + d * (1.0 - v + (log v)) then
                    d * v
                else gamma_sample()
            alphafix * gamma_sample() / beta  
        static member PDF alpha beta x = 
            gammaCheckParam alpha beta
            if x >= 0.0 then
                (beta**alpha) * (x ** (alpha - 1.0)) * (exp (-beta*x)) / Core.Gamma(alpha)
            else 0.0
        static member CDF alpha beta x =
            gammaCheckParam alpha beta
            if x < 0.0 then 0.0
            else failwith "Not implemented yet."
        static member Support alpha beta =
            gammaCheckParam alpha beta
            (0.0, System.Double.PositiveInfinity)
    
    /// Beta distribution
    type Beta =
        static member Mean alpha beta =
            gammaCheckParam alpha beta
            alpha / (alpha + beta)
        static member Variance alpha beta =
            gammaCheckParam alpha beta
            (alpha * beta) / ((alpha + beta) * (alpha + beta) * (alpha + beta + 1.0))
        static member StandardDeviation alpha beta =
            gammaCheckParam alpha beta
            sqrt ((alpha * beta) / ((alpha + beta) * (alpha + beta) * (alpha + beta + 1.0)))
        static member Sample alpha beta = 
            gammaCheckParam alpha beta
            let x = Gamma.Sample alpha 1.0
            let y = Gamma.Sample beta 1.0
            x / (x + y)
        static member PDF alpha beta x = 
            gammaCheckParam alpha beta
            if x >= 0.0 && x <= 1.0 then
                (x ** (alpha - 1.0)) * ((1.0 - x) ** (beta - 1.0)) / (Core.Beta alpha beta)
            else 0.0
        static member CDF alpha beta x =
            gammaCheckParam alpha beta
            if x < 0.0 then 0.0
            elif x > 1.0 then 1.0
            else failwith "Not implemented yet."
        static member Support alpha beta =
            gammaCheckParam alpha beta
            (0.0, 1.0)

    // Dirichlet distribution helper functions.
    let dirichletCheckParam (alpha: vector) =
        let ok = Vector.fold (fun acc a -> if a < 0.0 then false else acc) true alpha
        if (not ok) then failwith "Dirichlet distribution should be parametrized by a vector alpha > 0.0."
    
    /// Beta distribution
    type Dirichlet =
        static member Mean (alpha: vector) =
            dirichletCheckParam alpha
            let s = 1.0 / (Vector.sum alpha)
            alpha * s
        static member Covariance (alpha: vector) =
            dirichletCheckParam alpha
            let s = (Vector.sum alpha)
            let n = s * s * (s + 1.0)
            let p = Vector.length alpha
            Matrix.init p p (fun i j ->
                                if i = j then
                                    alpha.[i] * (s - alpha.[i]) / n
                                else
                                    - alpha.[i] * alpha.[j] / n)
        static member Sample (alpha: vector) =
            dirichletCheckParam alpha
            let p = Vector.length alpha
            let gv = Vector.init p (fun i -> Gamma.Sample alpha.[i] 1.0)
            let s = Vector.sum gv
            Vector.init p (fun i -> gv.[i] / s)
        static member PDF (alpha: vector) (x: vector) =
            dirichletCheckParam alpha
            if not (Vector.fold (fun acc a -> if a < 0.0 || a > 1.0 then false else acc) true alpha) then
                0.0
            else
                let t = Vector.foldi (fun i acc a -> acc * (x.[i] ** (alpha.[i] - 1.0)) / (Core.Gamma alpha.[i])) 1.0 alpha
                let s = (Vector.sum alpha)
                t * (Core.Gamma s)
        static member CDF (alpha: vector) (x: vector) =
            dirichletCheckParam alpha
            failwith "Not implemented yet."
            0.0
        static member Support (alpha: vector) =
            dirichletCheckParam alpha
            let p = Vector.length alpha
            Vector.Generic.create p (0.0, 1.0)