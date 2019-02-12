#light

/// This module features different functionality to read and write matrix formats.
module FSharp.MathTools.IO

    open Microsoft.FSharp.Math

    /// This module reads and writes matrices and vectors in tab separated format.
    module TextTSV =
        /// Reads matrix from a tsv file.
        val ReadMatrix : fileName:string -> Matrix<'a>
        /// Writes matrix to a tsv file.
        val WriteMatrix : fileName:string -> Matrix<'a> -> unit
        /// Reads vector from a tsv file.
        val ReadVector : fileName:string -> Vector<'a>
        /// Writes vector to a tsv file.
        val WriteVector : fileName:string -> Vector<'a> -> unit

    /// This module reads and writes matrices and vectors in comma separated format.
    module TextCSV =
        /// Reads matrix from a csv file.
        val ReadMatrix : fileName:string -> Matrix<'a>
        /// Writes matrix to a csv file.
        val WriteMatrix : fileName:string -> Matrix<'a> -> unit
        /// Reads vector from a csv file.
        val ReadVector : fileName:string -> Vector<'a>
        /// Writes vector to a csv file.
        val WriteVector : fileName:string -> Vector<'a> -> unit
        
    /// This module reads and writes named matrices and scalars in matlab format. Since matlab doesn't
    /// make a difference between vectors and matrices, all (row)vectors must be cast to matrices.
    module Matlab5 =
        type Workspace =
            { FloatMts: (string * Matrix<float>) list;
              ComplexMts: (string * Matrix<complex>)  list;
              Floats: (string * float)  list;
              Complex: (string * complex)  list;
              Ints: (string * int)  list }
        /// Reads a workspace from a matlab file.
        val Read : fileName:string -> Workspace
        /// Writes a workspace to a matlab file.
        val Write : fileName:string -> Workspace -> unit