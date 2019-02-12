#light

module FSharp.MathTools.Stat

open Microsoft.FSharp.Math



// Probability and statistics functions
//------------------------------------------------------------------------------------
/// Interface that every uniform random number generator must implement.
type IRandom =
    abstract NextInt : unit -> int
    abstract NextFloat : unit -> float

/// Sets the random number generator used for sampling.
val SetSampleGenerator : rg:IRandom -> unit
/// Returns the random number generator used for sampling.
val GetSampleGenerator : unit -> IRandom

/// A standard implementation of a uniform random source using System.Random()
type RandBasic =
    interface IRandom
    /// If n is negative, the random number generator seed is based on system time, if it is zero or positive it will
    /// use n as the seed.
    new : n:int -> RandBasic
    /// Constructs the default random number generator with seed 17.
    new : unit -> RandBasic


/// This module contains various probability distributions including methods to sample from them, evaluate pdf and cdf functions, ...
module Distributions =
  /// Uniform distribution: this distribution is parametrized by a lower and upper bound.
  type Uniform =
    class end
    with
     /// Computes the cumulative distribution function.
     static member CDF : min:float -> max:float -> (float -> float)
     /// Computes the mean.
     static member Mean : min:float -> max:float -> float
     /// Computes the probability density function.
     static member PDF : min:float -> max:float -> (float -> float)
     /// Produces a random sample using the current random number generator (from GetSampleGenerator()).
     static member Sample : min:float -> max:float -> float
     /// Computes the standard deviation.
     static member StandardDeviation : min:float -> max:float -> float
     /// Computes the variance.
     static member Variance : min:float -> max:float -> float
     /// Returns the support of the uniform distribution: [min, max].
     static member Support : min:float -> max:float -> float * float
    end
    
  /// The Bernoulli distribution: this distribution is parametrized by a real number p in [0,1].
  type Bernoulli =
    class end
    with
     /// Computes the cumulative distribution function.
     static member CDF : p:float -> (float -> float)
     /// Computes the mean.
     static member Mean : p:float -> float
     /// Computes the probability density function.
     static member PDF : p:float -> (float -> float)
     /// Produces a random sample using the current random number generator (from GetSampleGenerator()).
     static member Sample : p:float -> float
     /// Computes the standard deviation.
     static member StandardDeviation : p:float -> float
     /// Computes the variance.
     static member Variance : p:float -> float
    end
    
  /// The Multinomial distribution: this distribution is parametrized by a vector of probabilities p and a number n specifying
  /// the number of trials. The library does not check whether the vector of probabilities is properly normalized.
  type Multinomial =
    class end
    with
     /// Computes the mean.
     static member Mean : p:vector -> n:int -> vector
     /// Computes the probability density function.
     static member PDF : p:vector -> n:int -> x:Vector<int> -> float
     /// Produces a random sample using the current random number generator (from GetSampleGenerator()).
     static member Sample : p:vector -> n:int -> x:Vector<int>
     /// Computes the standard deviation.
     static member StandardDeviation : p:vector -> n:int -> vector
     /// Computes the variance.
     static member Variance : p:vector -> n:int -> vector
    end
    
  /// Normal distribution: this distribution is parametrized by a mean in (-inf,inf) and standard deviation [0,inf).
  type Normal =
    class end
    with
     /// Computes the cumulative distribution function.
     static member CDF : mu:float -> tau:float -> (float -> float)
     /// Computes the mean.
     static member Mean : mu:float -> tau:float -> float
     /// Computes the probability density function.
     static member PDF : mu:float -> tau:float -> (float -> float)
     /// Produces a random sample using the current random number generator (from GetSampleGenerator()).
     static member Sample : mu:float -> tau:float -> float
     /// Computes the standard deviation.
     static member StandardDeviation : mu:float -> tau:float -> float
     /// Computes the variance.
     static member Variance : mu:float -> tau:float -> float
     /// Returns the support of the normal distribution: (Negative Infinity, Positive Infinity).
     static member Support : mu:float -> tau:float -> float * float
    end
  
  /// The exponential distribution: this distribution is parametrized by a real number lambda > 0.
  type Exponential =
    class end
    with
     /// Computes the cumulative distribution function.
     static member CDF : lambda:float -> (float -> float)
     /// Computes the mean.
     static member Mean : lambda:float -> float
     /// Computes the probability density function.
     static member PDF : lambda:float -> (float -> float)
     /// Produces a random sample using the current random number generator (from GetSampleGenerator()).
     static member Sample : lambda:float -> float
     /// Computes the standard deviation.
     static member StandardDeviation : lambda:float -> float
     /// Computes the variance.
     static member Variance : lambda:float -> float
     /// Returns the support of the exponential distribution: [0, Positive Infinity).
     static member Support : lambda:float -> float * float
    end
  
  /// The gamma distribution: this distribution is parametrized by two real numbers alpha > 0, beta > 0.
  type Gamma =
    class end
    with
     /// Computes the cumulative distribution function.
     static member CDF : alpha:float -> beta:float -> (float -> float)
     /// Computes the mean.
     static member Mean : alpha:float -> beta:float -> float
     /// Computes the probability density function.
     static member PDF : alpha:float -> beta:float -> (float -> float)
     /// Produces a random sample using the current random number generator (from GetSampleGenerator()).
     static member Sample : alpha:float -> beta:float -> float
     /// Computes the standard deviation.
     static member StandardDeviation : alpha:float -> beta:float -> float
     /// Computes the variance.
     static member Variance : alpha:float -> beta:float -> float
     /// Returns the support of the gamma distribution: [0, Positive Infinity).
     static member Support : alpha:float -> beta:float -> float * float
    end
  
  /// The beta distribution: this distribution is parametrized by two real numbers alpha > 0, beta > 0.
  type Beta =
    class end
    with
     /// Computes the cumulative distribution function.
     static member CDF : alpha:float -> beta:float -> (float -> float)
     /// Computes the mean.
     static member Mean : alpha:float -> beta:float -> float
     /// Computes the probability density function.
     static member PDF : alpha:float -> beta:float -> (float -> float)
     /// Produces a random sample using the current random number generator (from GetSampleGenerator()).
     static member Sample : alpha:float -> beta:float -> float
     /// Computes the standard deviation.
     static member StandardDeviation : alpha:float -> beta:float -> float
     /// Computes the variance.
     static member Variance : alpha:float -> beta:float -> float
     /// Returns the support of the beta distribution: [0, 1].
     static member Support : alpha:float -> beta:float -> float * float
    end
  
  /// The Dirichlet distribution: this distribution is parametrized by a vector alpha >= 0.
  type Dirichlet =
    class end
    with
     /// Computes the cumulative distribution function.
     static member CDF : alpha:vector -> (vector -> float)
     /// Computes the mean.
     static member Mean : alpha:vector -> vector
     /// Computes the probability density function.
     static member PDF : alpha:vector -> (vector -> float)
     /// Produces a random sample using the current random number generator (from GetSampleGenerator()).
     static member Sample : alpha:vector -> vector
     /// Computes the covariance matrix.
     static member Covariance : alpha:vector -> matrix
     /// Returns the support of the Dirichlet distribution: [0, 1].
     static member Support : alpha:vector -> Vector<float * float>
    end