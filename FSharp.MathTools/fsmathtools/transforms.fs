#light

/// This namespace groups many possible function transformations.
namespace FSharp.MathTools.Transforms

open Microsoft.FSharp.Math

/// This module implements Fourier transformations. The FSharp.MathTools library does not implement
/// Fourier transforms itself but provides bindings to the FFTW library: http://www.fftw.org/.
module Fourier =

    open System.Runtime.InteropServices
    open Microsoft.FSharp.NativeInterop
    
    /// This module provides .NET bindings to the FFTW library.
    module FFTW =
        [<DllImport(@"libfftw3-3.dll", EntryPoint="fftw_malloc")>]
        extern double* fftw_malloc(int size);

        [<DllImport(@"libfftw3-3.dll", EntryPoint="fftw_free")>]
        extern void fftw_free(double* data);

        [<DllImport(@"libfftw3-3.dll", EntryPoint="fftw_plan_dft_1d")>]
        extern void* fftw_plan_dft_1d(int n, double* i, double* o, int kind, int flags);

        [<DllImport(@"libfftw3-3.dll", EntryPoint="fftw_destroy_plan")>]
        extern void destroy_plan(void* plan);

        [<DllImport(@"libfftw3-3.dll", EntryPoint="fftw_execute")>]
        extern void execute(void* plan);
        
    // This helper function creates an n by 2 array where the columns contain respectively the real and imaginary parts of a complex array.
    let prepare (ca: Complex array) = Array2.init ca.Length 2 (fun i j -> if j = 0 then (ca.[i]).r else (ca.[i]).i )
    
    
   
    /// This function takes a complex array and copies the content into an FFTW allocated memory buffer. It returns the discrete Fourier
    /// transform as a new complex array. The current implementation uses an in-place FFTW Fourier transform.
    let InternalFFT (carr: complex array) (inverse: bool) =
        let n = carr.Length
        // Allocate memory for the array and fill it with the right numbers.
        let ptr = FFTW.fftw_malloc(16*n)
        // Create an FFTW plan and execute.
        let plan = if inverse then
                        FFTW.fftw_plan_dft_1d(n, ptr, ptr, 1, 0)
                   else
                        FFTW.fftw_plan_dft_1d(n, ptr, ptr, -1, 0)
        let narr = NativeArray2.FromPtr(ptr, n, 2)
        Array.iteri (fun i (x: Complex) -> (*DEBUG printf "setting %d,%d to %f\n" i j x;*) narr.[i,0] <- x.r; narr.[i,1] <- x.i) carr
        FFTW.execute plan
        // Create output array.
        let r = Array.init n (fun i -> Complex.Create(narr.[i,0], narr.[i,1]))
        // Clean up.
        FFTW.fftw_free ptr
        FFTW.destroy_plan plan
        r
    
    /// Forward fast fourier transform.
    let FFT (arr: 'a array): array<complex> =
        match box arr with 
        | (:? array<float> as farr) -> InternalFFT (Array.map (fun f -> Complex.Create(f,0.0)) farr) false
        | (:? array<complex> as carr) -> InternalFFT carr false
        | _               -> failwith "Numerical format not implemented."
        
            
    /// Inverse fast fourier transform.
    let InverseFFT (arr: 'a array): array<complex> =
        let n = complex (float arr.Length) 0.0
        match box arr with 
        | (:? array<float> as farr) -> Array.map (fun x -> x / n) (InternalFFT (Array.map (fun f -> Complex.Create(f,0.0)) farr) true)
        | (:? array<complex> as carr) ->Array.map (fun x -> x / n) (InternalFFT carr true)
        | _               -> failwith "Numerical format not implemented."

    /// Computes the convolution of two real vectors.
    let Convolution (a:float array) (b:float array) =
        let na = a.Length
        let nb = b.Length
        let ffta = FFT (Array.init (na+nb-1) (fun i -> if i < na then Complex.Create(a.[i],0.0) else Complex.Zero))
        let fftb = FFT (Array.init (na+nb-1) (fun i -> if i < nb then Complex.Create(b.[i],0.0) else Complex.Zero))
        let ifft = InverseFFT (Array.map2 (fun ta tb -> ta * tb) ffta fftb)
        Array.map (fun (x:Complex) -> x.r) ifft