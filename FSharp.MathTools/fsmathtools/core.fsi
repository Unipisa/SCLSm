#light

module FSharp.MathTools.Core



// Standard mathematical constants
//------------------------------------------------------------------------------------
/// The constant pi = 3.141596...
val PI : float
/// The constant e = 2.718281828...
val E : float



// Standard mathematical functions
//------------------------------------------------------------------------------------
/// Computes the gamma function.
val Gamma : float -> float
/// Computes the natural logarithm of the gamma function.
val GammaLn : float -> float

/// The factorial functions takes an int x and computes x!. This function will not overflow
/// the floating point format as long as x is at most 170.
val Factorial : int -> float
/// Computes the natural logarithm of the factorial function.
val FactorialLn : int -> float
/// Computes the binomial coefficient.
val Binomial : int -> int -> float
/// Computes the multinomial coefficient.
val Multinomial : int -> int array -> float
/// Computes the natural logarithm of the beta function.
val BetaLn : float -> float -> float
/// Computes the beta function.
val Beta : float -> float -> float
/// Computes the error function. Note that this implementation has only been verified to have a relative error of around 1e-5.
val Erf : float -> float
/// Computes the complement of the error function. Note that this implementation has only been verified to have a relative error of around 1e-4.
val Erfc : float -> float



// Utility functions
//------------------------------------------------------------------------------------
/// Computes the cumulative sum of an array.
val CumulativeSum : float array -> float array
/// Computes a histogram with n bins for an array of numbers. The method returns the counts and means for the bins.
val Histogram : float array -> n:int -> int array * float array