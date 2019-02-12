#light


module FSharp.MathTools.Compatibility.Matlab

// Standard mathematical constants
//------------------------------------------------------------------------------------
/// The constant pi = 3.141596...
val pi : float
/// Float infinity.
val inf : float
/// Float NaN.
val NaN : float
/// Returns the logarithm for x in base 2.
val log2 : float -> float
/// Returns the logarithm for x in base 10.
val log10 : float -> float


// Functions for statistics and probability
//------------------------------------------------------------------------------------
/// Generates a uniformly distributed random floating point number in the interval [0,1].
val rand : unit -> float
/// Generates a normal distributed random floating point number.
val randn : unit -> float
/// Generates a random permutation of the numbers 0..n-1 using a Knuth shuffle.
val randperm : int -> int array
/// Returns the value for the density function of the normal probability distribution with
/// mean mu and standard deviation tau.
val normpdf : float -> float -> float -> float
/// Returns a random number from a normal distribution with mean mu and standard deviation sdev.
val normrand : float -> float -> float
/// Computes the cumulative sum of the elements in an array.
val cumsum : float array -> float array


// Matrix and vector helper functions
//------------------------------------------------------------------------------------
/// Generates a rowvector of n linearly spaced points between and including a and b.
val linspace : a:float -> b:float -> n:int -> rowvec

/// Make a matrix which replicate the matrix A, m times along the row dimension and n times along the column dimension.
val repmat : A:matrix -> m:int -> n:int -> matrix


// Utility helper functions
//------------------------------------------------------------------------------------
/// Saves the current time. Use toc to get elapsed time.
val tic : unit -> unit
/// Returns the time in seconds since calling toc.
val toc : unit -> float
    