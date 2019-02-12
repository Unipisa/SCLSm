#light

module FSharp.MathTools.IO

    open System
    open System.IO
    open Microsoft.FSharp.Math
    open Microsoft.FSharp.Reflection
    
    type System.IO.FileStream with
        /// Augment the FileStream class with a method to conveniently return an array of bytes read and the number of bytes read.
        member this.ReadBytes n =
            let b = Array.create n (byte 0)
            let n = this.Read(b, 0, n)
            (n,b)

    // TODO adjust the precision with which floats are written to file
    let SerializeInt (i:int) = sprintf "%d" i
    let SerializeFloat (f:float) = sprintf "%.16e" f
    let SerializeComplex (c:Complex) = c.ToString()
    let ParseInt (s:string) = System.Int32.Parse(s)
    let ParseFloat (s:string) = System.Double.Parse(s)
    let ParseComplex (s:string) = match String.split ['+';'i';'r'] s with
                                  | r::i::_ -> Complex.Create (System.Double.Parse(r), System.Double.Parse(i))
                                  | _ -> failwith "Bad file format."

    let WriteMatrixToText (delim:string) (fileName:string) (A:Matrix<'a>) =
        let (n,m) = A.Dimensions
        let sw = new StreamWriter(fileName)
        for i=0 to n-1 do
            for j=0 to m-1 do
                match (box A.[i,j]) with
                | :? float as x -> sw.Write((SerializeFloat x) + delim)
                | :? complex as x-> sw.Write((SerializeComplex x) + delim)
                | _ -> failwith "Don't know how to serialize the matrix element type."
            sw.Write("\n")
        sw.Close()

    let cast x = unbox (box x)
    let ReadMatrixFromText (delim:string) (fileName:string) : Matrix<'a> =
        let lines = Seq.generate_using (fun () -> new StreamReader(fileName))
                                       (fun file -> if (file.EndOfStream) then None else Some(file.ReadLine()))
        let unpack (line:string) : 'a list =
            let words = line.Split([|delim|], StringSplitOptions.RemoveEmptyEntries)
            [ for w in words -> match () with
                                | _ when typeof<'a> = typeof<float> -> cast (ParseFloat w)
                                | _ when typeof<'a> = typeof<complex> -> cast (ParseComplex w)
                                | _ -> failwith "Don't know how to parse the matrix element type." ]
        let filter line = if line = "" then false else true
        lines |> Seq.filter filter |> Seq.map unpack |> List.of_seq |> Matrix.Generic.of_list


    module TextTSV =
        let ReadMatrix fileName = ReadMatrixFromText "\t" fileName
        let WriteMatrix fileName A = WriteMatrixToText "\t" fileName A
        let ReadVector fileName = Matrix.Generic.getCol (ReadMatrixFromText "\t" fileName) 0
        let WriteVector fileName v = WriteMatrixToText "\t" fileName (Matrix.Generic.of_vector v)

    module TextCSV =
        let ReadMatrix fileName = ReadMatrixFromText ", " fileName
        let WriteMatrix fileName A = WriteMatrixToText ", " fileName A
        let ReadVector fileName = Matrix.Generic.getCol (ReadMatrixFromText ", " fileName) 0
        let WriteVector fileName v = WriteMatrixToText ", " fileName (Matrix.Generic.of_vector v)
        
    module Matlab5 =
    
        type Workspace =
            { FloatMts: (string * Matrix<float>) list;
              ComplexMts: (string * Matrix<complex>)  list;
              Floats: (string * float)  list;
              Complex: (string * complex)  list;
              Ints: (string * int)  list }
        
        let Read (fileName:string) : Workspace =
            /// Reads the matlab header and returns a method that returns data in little endian format.
            let ReadHeader (fs:FileStream): (int -> int * byte array) =
                let (n, hdb) = fs.ReadBytes 128
                if hdb.[126] = (Convert.ToByte 'I') then fs.ReadBytes
                                                    else (fun n -> let (l,data) = fs.ReadBytes n
                                                                   l, Array.init l (fun i -> let r = i % 4
                                                                                             match r with
                                                                                             | 0 -> data.[i + 3]
                                                                                             | 1 -> data.[i + 1]
                                                                                             | 2 -> data.[i - 1]
                                                                                             | 3 -> data.[i - 3]
                                                                                             | _ -> failwith "Cannot get here." ))
                                                    
            /// Parses a variable name block.
            let ParseName (data: byte array) =
                if data.[2] = 0uy && data.[3] = 0uy then            //  Small data element format
                    let nameLength = BitConverter.ToInt32(data, 4)
                    (new String(Array.map (fun x -> Byte.to_char x) data.[8 .. 8+nameLength-1])).Trim().Trim([|'\000'|]), nameLength
                else
                    let nameLength = int (BitConverter.ToInt16(data, 0))
                    (new String(Array.map (fun x -> Byte.to_char x) data.[4 .. 4+nameLength-1])).Trim().Trim([|'\000'|]), 0
            
            /// Parses a 2D array block.
            let Parse2DArray (data: byte array) n m =
                // TODO first 8 bytes specify digit format. Check if this could be different than 4 bytes per matrix entry.
                let M = Matrix.zero n m
                for i = 0 to (m - 1) do 
                    for j = 0 to (n - 1) do
                        M.[j,i] <- BitConverter.ToDouble(data, 8+8*(n*i + j))
                M
            
            /// Parses the bytes to return a float matrix. This function expects all bytes in the data element starting from the array flags.
            let ParseFloatMatrix data : string * Matrix<float>  =
                // Read the matrix dimensions.
                let nrows = BitConverter.ToInt32(data, 24)
                let ncols = BitConverter.ToInt32(data, 28)
                
                let name, length = ParseName data.[32 ..]                           //  Remember the matrix name
                
                name, (Parse2DArray data.[40 + length ..] nrows ncols)
                
            /// Parses the bytes to return a complex matrix. This function expects all bytes in the data element starting from the array flags.
            let ParseComplexMatrix data : string * Matrix<complex> =
                // Read the matrix dimensions.
                let nrows = BitConverter.ToInt32(data, 24)
                let ncols = BitConverter.ToInt32(data, 28)
                
                let name, length = ParseName data.[32 ..]                           //  Remember the matrix name
                
                let Mreal = Parse2DArray data.[40 + length ..] nrows ncols
                let Mimag = Parse2DArray data.[40 + length + 8 + (nrows * ncols * 8) ..] nrows ncols
                name, Matrix.Generic.init nrows ncols (fun i j -> Complex.Create(Mreal.[i,j], Mimag.[i,j]))
            
            /// Loops over the whole file, reads all data elements and parses them.
            let rec ReadDataElements (reader: int -> int * byte array) (content: Workspace) : Workspace =
                let (tn, td) = reader 4
                let (ln, ld) = reader 4
                if tn = 0 then content
                else
                    let (dn, dd) = reader (BitConverter.ToInt32(ld,0))
                    
                    // Read the data type fields
                    match BitConverter.ToInt32(td, 0) with
                    | 14 (* miMATRIX *) ->
                        //let _ = BitConverter.ToInt32(dd, 4)                             // number of bytes
                        
                        // parse the array flags
                        // TODO also read the array type (mxDOUBLE_CLASS ...)
                        if ((dd.[9] >>> 3) &&& 1uy) = 1uy then                             // Check whether matrix is complex or not
                            ReadDataElements reader { FloatMts = content.FloatMts;
                                                      ComplexMts = (ParseComplexMatrix dd) :: content.ComplexMts;
                                                      Floats = content.Floats;
                                                      Complex = content.Complex;
                                                      Ints = content.Ints }
                        else
                            ReadDataElements reader { FloatMts = (ParseFloatMatrix dd) :: content.FloatMts;
                                                      ComplexMts = content.ComplexMts;
                                                      Floats = content.Floats;
                                                      Complex = content.Complex;
                                                      Ints = content.Ints }
                    | _ as x -> failwithf "Data element parsing not implemented %d" x
            
            let fs = new FileStream(fileName, FileMode.Open)
            let reader = ReadHeader fs
            let contents = ReadDataElements reader { FloatMts = []; ComplexMts = []; Floats = []; Complex = []; Ints = [] }
            fs.Close()
            contents
            
        /// Saves a list of matrices to a MATLAB file.
        let Write (fileName:string) (content: Workspace) =
        
            // Generates the MATLAB V5 compatible header.
            let MatlabHeader comments = 
                let n = String.length comments 
                let HeaderByte i = 
                    match i with 
                    | 124 -> 0uy
                    | 125 -> 1uy
                    | 126 -> Convert.ToByte ('I')
                    | 127 -> Convert.ToByte ('M')
                    | i when i < 128 -> if (i >= n) then 0uy else Convert.ToByte (comments.[i])
                    | _ -> failwith "Matlab header to long."
                Array.init 128 HeaderByte
              
            // Generates a MATLAB V5 compatible data element (matrix).
            let MatlabDataElement (m:Matrix<'a>) (name:string) =
                let e = (box m.[0,0])
                
                // replaces all non-alphanumerical characters with underscores
                let ValidVariableName ca = 
                    ca |> Array.map (fun c -> if ((Char.IsDigit c) || (Char.IsLetter c)) then c else '_') 
                
                // pads a char array to be a mulitple of n in length
                let PadString n ca = 
                    let l = Array.length ca 
                    Array.init (l + (n - l % n )) (fun i -> if (i < l) then ca.(i) else '\000') 
                    
                let realName = name.ToCharArray () |> ValidVariableName |> PadString 8
                let buffer =  match e with
                              | :? float -> Array.create (8 * m.NumRows * m.NumCols + 56 + realName.Length) 0uy
                              | :? complex -> Array.create (2 * 8 * m.NumRows * m.NumCols + 64 + realName.Length) 0uy
                              | _ -> failwith "Only float and complex matrices can be serialized to the Matlab file format now."
                 

                // write the data type fields
                do (BitConverter.GetBytes (14)).CopyTo (buffer, 0)                          // 14 == miMATRIX
                do (BitConverter.GetBytes (buffer.Length - 8)).CopyTo (buffer, 4)           // number of bytes 

                // write the array flags 
                do (BitConverter.GetBytes (6)).CopyTo (buffer, 8)                           // 6 == miUINT32
                do (BitConverter.GetBytes (8)).CopyTo (buffer, 12)                          // 8 bytes to follow
                do (BitConverter.GetBytes (6)).CopyTo (buffer, 16)                          // 6 == mxDOUBLE_CLASS 
                do match e with
                   | :? complex -> (BitConverter.GetBytes (8)).CopyTo (buffer, 17)
                   | _ -> ()

                // write the dimension field
                do (BitConverter.GetBytes (5)).CopyTo (buffer, 24)                          // 5 == miINT32          
                do (BitConverter.GetBytes (8)).CopyTo (buffer, 28)                          // 8 bytes to follow         
                do (BitConverter.GetBytes (m.NumRows)).CopyTo (buffer, 32)                  // number of rows         
                do (BitConverter.GetBytes (m.NumCols)).CopyTo (buffer, 36)                  // number of columns

                 // write the matrix name
                do (BitConverter.GetBytes (1)).CopyTo (buffer, 40)                          // 1 == miINT8          
                do (BitConverter.GetBytes (realName.Length)).CopyTo (buffer, 44)            // length of name in bytes         
                for i = 0 to (realName.Length - 1) do                                       // write tne name of the matrix
                    buffer.[48 + i] <- Convert.ToByte (realName.[i])
                done                 
                let pt = 48 + realName.Length
                
                // write the real part of the matrix data
                do (BitConverter.GetBytes (9)).CopyTo (buffer, pt)                          // 1 == miDOUBLE          
                do (BitConverter.GetBytes (8*m.NumRows*m.NumCols)).CopyTo (buffer, pt+4)    // length of data buffer
                let mutable k = pt + 8 
                for i = 0 to (m.NumCols - 1) do 
                    for j = 0 to (m.NumRows - 1) do
                        do match (box m.[j,i]) with
                           | :? float as x -> (BitConverter.GetBytes (x)).CopyTo (buffer, k)
                           | :? complex as x -> (BitConverter.GetBytes (Complex.realPart x)).CopyTo (buffer, k)
                           | _ -> failwith "Only float and complex matrices can be serialized to the Matlab file format now."
                        k <- k + 8
                    done
                done
                
                // write the complex part of the matrix data
                if (e :? complex) then
                    do (BitConverter.GetBytes (9)).CopyTo (buffer, k)                          // 1 == miDOUBLE          
                    do (BitConverter.GetBytes (8*m.NumRows*m.NumCols)).CopyTo (buffer, k+4)    // length of data buffer
                    k <- k + 8 
                    for i = 0 to (m.NumCols - 1) do 
                        for j = 0 to (m.NumRows - 1) do
                            do match (box m.[j,i]) with
                               | :? complex as x -> (BitConverter.GetBytes (Complex.imagPart x)).CopyTo (buffer, k)
                               | _ -> failwith "Only float and complex matrices can be serialized to the Matlab file format now."
                            k <- k + 8
                        done
                    done 
                buffer

            // opens the writer for MATLAB files
            let writer = new FileStream(fileName, FileMode.Create) 
            // implements a quick write-of-byte-buffer function
            let write b = writer.Write (b, 0, b.Length) 

            // write header ...
            do write (MatlabHeader (sprintf "Generated by F# Math Tools on %A." System.DateTime.Now))
            // ... and content
            content.ComplexMts |> List.iter (fun (name,m) -> write (MatlabDataElement m name))
            content.FloatMts |> List.iter (fun (name,m) -> write (MatlabDataElement m name))
            //TODO content.Complex
            //TODO content.Floats
            //TODO content.Ints
            writer.Close ()