#light

module FSharp.MathTools.Notation

// Additions to the F# math libraries.
//------------------------------------------------------------------------------------ 
type Microsoft.FSharp.Math.Vector<'a> with
    /// Vector transpose.
    member T : Microsoft.FSharp.Math.RowVector<'a>
    
type Microsoft.FSharp.Math.RowVector<'a> with
    /// RowVector transpose.
    member T : Microsoft.FSharp.Math.Vector<'a>
    
type Microsoft.FSharp.Math.Matrix<'a> with
    /// Matrix transpose.
    member T : Microsoft.FSharp.Math.Matrix<'a>