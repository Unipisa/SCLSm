#light

module FSharp.MathTools.Notation


// Additions for the F# math libraries.
//------------------------------------------------------------------------------------
type Microsoft.FSharp.Math.Vector with
    /// Vector transpose.
    member this.T = this.Transpose
    
type Microsoft.FSharp.Math.RowVector with
    /// RowVector transpose
    member this.T = this.Transpose
    
type Microsoft.FSharp.Math.Matrix with
    /// Matrix transpose.
    member this.T = this.Transpose