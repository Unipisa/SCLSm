#light
#nowarn "62"

open System
open System.Collections
open System.Collections.Generic
open System.IO
//open Microsoft.FSharp.Idioms
open Microsoft.FSharp.Math
//open Microsoft.FSharp.Math.BigInt
open System.Text;
open System
open System.Threading

open SCLS

(*
#r @"..\SCLS\scls.dll";;
#r @"..\SCLS_Parser\bin\Relase\SCLS_Parser.dll";;
#r @"..\CodeExpressionEvaluator\bin\Relase\CodeExpressionEvaluator.dll";; 
*)
let show_usage() = 
        Console.WriteLine("Usage : ")
        Console.WriteLine("        - SCLS.exe <input_file_path> <time_limit> [<output_file_path> <time_sampling_rate>] [-p X|-c|-cp]")
        Console.WriteLine("             * -p X      = execute X simulations in parallel and give the mean")
        Console.WriteLine("             * -c        = use constant seach for ground rules")
        Console.WriteLine("             * -cp       = like -c but find matches in paralle (useful with great number of ground rules)")
        //Console.WriteLine("             * -b        = print benchmark information")

let exit() =
        System.Environment.Exit(1)

let open_file(filename:string) =
        let (t:FileInfo) = new FileInfo(filename);
        let  (tex:StreamWriter) =t.CreateText();
        tex

let add_to_file(tex:StreamWriter,data:float * int64 []) =
        /// here assume that each column take exactly TAB_CHAR characters
        let TAB_CHAR = 15
        let time_str = (fst(data).ToString())
        tex.Write(((time_str).Substring(0,Math.Min(10,time_str.Length))).PadRight(TAB_CHAR) + "\t");
        for d in snd(data) do
                            let d_str = d.ToString()
                            let value = ( (d.ToString()).Substring(0, (Math.Min(TAB_CHAR, d_str.Length)))).PadRight(TAB_CHAR) + "\t" 
                            tex.Write(value)
            done
        tex.Write("\n");
        tex.Flush();
        ()


let initializeOutput(output_filename, engine: Engine_pp.Engine) =
        let tex = open_file(output_filename)
        let model = engine.model
        /// here assume that each column take exactly TAB_CHAR characters
        let TAB_CHAR = 15
        
        /// initialize output and write the header 
        let mutable output = new System.Text.StringBuilder("time".PadRight(TAB_CHAR) + "\t")
        for keyVal in model.symbols.table do
                //if not (model.exclude.Count = model.symbols.Count()) && not (model.variables.Contains(keyVal.Key)) && not (model.exclude.Contains(keyVal.Key))
                //    then
                let value = let v = (model.symbols.table.[keyVal.Key].ToString()) in v.Substring( 0, (Math.Min(TAB_CHAR, v.Length)))  ;
                output.Append(value.PadRight(TAB_CHAR) + "\t") |> ignore
                //    else ()
        
        done
        for patt in model.patterns do
            let value = (patt.name).Substring( 0,  (Math.Min(TAB_CHAR, patt.name.Length)))  ;
            output.Append(value.PadRight(TAB_CHAR)+ "\t" ) |> ignore
        done
        output.Append("\n") |> ignore
        tex.Write(output.ToString())
        // starting state
        
        //output.Append("0,0".PadRight(10) + "\t") |> ignore
        let initial_data = engine.getStartConcentrations(ref 0.0, true)
        add_to_file(tex, initial_data)
        tex
        


let close_file(tex:StreamWriter) =
        tex.Close();
 
let args = System.Environment.GetCommandLineArgs()
let simulate_withoutoutput()=
                   
                    let input_filename =args.[1]
                    let time_limit = 
                        let ok,res = System.Double.TryParse((args.[2]).Replace(".",",")) 
                        if ok 
                            then
                                res
                            else 
                                show_usage()
                                exit()
                                0.0
                    let model = SCLS.SCLS_Parser.SCLS_Parser.Parse(input_filename)
                    let engine = new Engine_pp.Engine(model,false,true,false,false)
                    let clock = ref 0.0
                    while !clock < time_limit do
                        (engine.getEnumConcentrations(clock,false) |> ignore )
                    done
                    ()
let simulate_withoutput()=
                    let input_filename = args.[1]
                    let time_limit = 
                        let ok,res = System.Double.TryParse((args.[2]).Replace(".",",")) 
                        if ok 
                            then
                                res
                            else 
                                show_usage()
                                exit()
                                0.0
                    let output_filename = args.[3]
                    let data_refresh_rate = 
                        let ok,res = System.Double.TryParse((args.[4]).Replace(".",",")) 
                        if ok 
                            then
                                res
                            else 
                                show_usage()
                                exit()
                                0.0
                    let model = SCLS.SCLS_Parser.SCLS_Parser.Parse(input_filename)
                    let engine = new Engine_pp.Engine(model,false,true,false,false)
                    let clock = ref 0.0
                    let output = initializeOutput(output_filename,engine)
                    while !clock < time_limit do
                        let inner_limit = !clock + data_refresh_rate
                        while !clock < inner_limit do
                            (engine.getEnumConcentrations(clock,false) |> ignore )
                            done
                        let data = engine.getEnumConcentrations(clock,true)
                        add_to_file(output,data)
                    done
                    close_file(output)
                    ()
                    
let simulate_withoutput_constant(useParallel)=
                    let input_filename = args.[1]
                    let time_limit = 
                        let ok,res = System.Double.TryParse((args.[2]).Replace(".",",")) 
                        if ok 
                            then
                                res
                            else 
                                show_usage()
                                exit()
                                0.0
                    //Console.WriteLine(time_limit.ToString())
                    let output_filename = args.[3]
                    let data_refresh_rate = 
                        let ok,res = System.Double.TryParse((args.[4]).Replace(".",",")) 
                        if ok 
                            then
                                res
                            else 
                                show_usage()
                                exit()
                                0.0
                    let model = SCLS.SCLS_Parser.SCLS_Parser.Parse(input_filename)
                    let engine = new Engine_pp.Engine(model,true,true,false,useParallel) // <- use constant search
                    let clock = ref 0.0
                    let output = initializeOutput(output_filename,engine)
                    while !clock < time_limit do
                        let inner_limit = !clock + data_refresh_rate
                        while !clock < inner_limit do
                            (engine.getEnumConcentrations(clock,false) |> ignore )
                            done
                        let data = engine.getEnumConcentrations(clock,true)
                        add_to_file(output,data)
                    done
                    close_file(output)
                    ()


let simulate_withoutput_parallel() =
                    //SCLS.exe <input_file_path> <time_limit> <output_file_path> <time_sampling_rate> -p X
                    let input_filename = args.[1]
                    let time_limit = 
                        let ok,res = System.Double.TryParse((args.[2]).Replace(".",",")) 
                        if ok 
                            then
                                res
                            else 
                                show_usage()
                                exit()
                                0.0
                    let output_filename = args.[3]
                    let data_refresh_rate = 
                        let ok,res = System.Double.TryParse((args.[4]).Replace(".",",")) 
                        if ok 
                            then
                                res
                            else 
                                show_usage()
                                exit()
                                0.0
                    let number_of_runs = 
                        let ok,res = System.Int32.TryParse(args.[6]) 
                        if ok 
                            then
                                res
                            else 
                                show_usage()
                                exit()
                                0
                    let model_array = 
                        let a = Array.zeroCreate number_of_runs
                        for i = 0 to number_of_runs - 1 do a.[i] <-  SCLS.SCLS_Parser.SCLS_Parser.Parse(input_filename)
                        a
                        
                    let engine_array = 
                        let a = Array.zeroCreate number_of_runs
                        for i = 0 to number_of_runs - 1 do a.[i] <-  new Engine_pp.Engine(model_array.[i],false,true,false,false);
                        a
                        
                    let clock_array = 
                        let a = Array.zeroCreate number_of_runs
                        for i = 0 to number_of_runs - 1 do a.[i] <-  0.0
                        a
                    let task_bs i innerLimit =  async { do (
                                                            while clock_array.[i] < innerLimit do
                                                                let a = ref (clock_array.[i]) 
                                                                in (*print_any((!a).ToString());*) 
                                                                engine_array.[i].getEnumConcentrations(a,false)|> ignore; 
                                                                (*print_any(" " +(!a).ToString());*) clock_array.[i] <- !a; (*Console.WriteLine(" " +(clock_array.[i]).ToString());*)
                                                                done
                                                            )}
                    let taskWithOutOutput_BigStep(innerLimit) = [for i in 0 .. number_of_runs-1 -> (task_bs i innerLimit)] 
                    
                    let task i = async { do (
                                                let a = ref (clock_array.[i]) 
                                                in (*print_any((!a).ToString());*) 
                                                engine_array.[i].getEnumConcentrations(a,false)|> ignore; 
                                                (*print_any(" " +(!a).ToString());*) clock_array.[i] <- !a; (*Console.WriteLine(" " +(clock_array.[i]).ToString());*))}
                    let taskWithOutOutput = [for i in 0 .. number_of_runs-1 -> task i] 
                    
                    
                    
                    //initialize array of simulator
                    let output = initializeOutput(output_filename,engine_array.[0])
                    
                    let global_clock = ref 0.0
                    
                    while !global_clock < time_limit do
                        let inner_limit = !global_clock + data_refresh_rate
                        let data_array = Array.zeroCreate number_of_runs
                        Async.RunSynchronously (Async.Parallel (taskWithOutOutput_BigStep(inner_limit))) |> ignore
                        global_clock := clock_array.[0]
                        let data_array = Array.mapi (fun (i:int) (d:float) -> engine_array.[i].getEnumConcentrations(ref clock_array.[i],true)) data_array
                        
                        let result_clock = ( Array.fold (fun (acc) (d:float,_) -> acc +  d) (0.0) data_array)  / (float number_of_runs)
                        //mean of time
                        let result_data =
                            let n = data_array.Length
                            let m = (snd(data_array.[0])).Length
                            let result = Array.zeroCreate m
                            for i = 0 to m-1 do
                                for j = 0 to n-1 do
                                    let c = snd(data_array.[j])
                                    result.[i] <- (result.[i]) + (c.[i])
                                    
                                    done 
                                result.[i] <- (result.[i]) / (int64 n)
                                ()
                                done
                            result
                        //mean of data
                        global_clock := result_clock
                        let data = result_clock, result_data
                        add_to_file(output,data)
                        ()
                    done
                    close_file(output)
                    ()



    
      
let go() =
        //Console.WriteLine args.[0] // the program name
        //Array.iter (printf "%s ") args // print all
        //System.Environment.CommandLine
        
        let out = new StringBuilder()
        
        match args.Length
            with 
                | 1 | 2 ->  
                    show_usage()
                | 3 -> //simulate without saving of output //SCLS <input> <timelimit>
                    simulate_withoutoutput()
                | 5 ->
                    simulate_withoutput()
                | _ ->
                    match args.[5] with
                        | "-p" ->  simulate_withoutput_parallel()
                        | "-c" ->  simulate_withoutput_constant(false)
                        | "-cp" ->  simulate_withoutput_constant(true)
                        | _ -> show_usage()
                    //-p X  --> mean of X parallel execution
                    
                    //-b    --> benchmark
                    
                    
                    
        ()
        
let _ =
    /// Measures the time it takes to execute a function
    let timer = System.Diagnostics.Stopwatch.StartNew ()

    go()

    printfn "Time taken = %O" timer.Elapsed
                  
                            
        

