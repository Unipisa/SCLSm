#light

/// This namespace groups many possible function transformations.
namespace FSharp.MathTools.Transforms

open Microsoft.FSharp.Math

/// This module implements Fourier transformations. The FSharp.MathTools library does not implement
/// Fourier transforms itself but provides bindings to the FFTW library: http://www.fftw.org/.
module Fourier =
  /// Forward fast fourier transform; this method currently only supports float or complex input arrays.
  val FFT : 'a array -> Complex array
  /// Inverse fast fourier transform; this method currently only supports float or complex input arrays.
  val InverseFFT : 'a array -> Complex array
  /// Computes the convolution of two real vectors.
  val Convolution : float array -> float array -> float array