#light
#nowarn "62"

namespace SCLS

open System
open System.Threading
open System.Collections.Generic
open SCLS
open SCLS.Splasher
//open Microsoft.FSharp.Idioms
open System.Drawing;
open System.Timers;
open System.Windows.Forms
 
module Worker =
    begin



        type msg = 
            | Run 
            | Exit
            | Pause
            | Step
            | Reset
            
        let mutable mono = false
        let t = System.Type.GetType ("Mono.Runtime")
        in match t with
            | null ->()
            | _ -> mono <- true

        let mapTermToTreeNode (t:Compartment) symbols =
            let rec mapTermToTreeNodeInner (t: Node) (n:Int64) (node : TreeNode) =
                match t with
                    | :? Compartment as comp ->
                            let nodeString = comp.ToString(symbols)
                            let newNode = new TreeNode( nodeString+   " * " + n.ToString() )
                            newNode.ToolTipText <- nodeString
                            newNode.BackColor <- Color.LightGreen
                            node.Nodes.Add(newNode) |> ignore
                            for keyVal in Array.ofSeq comp.children do
                                mapTermToTreeNodeInner keyVal.Key keyVal.Value newNode
                            done
                    | :? Loop as loop ->
                            let nodeString = loop.ToString(symbols)
                            let newNode = new TreeNode(nodeString + " * "+n.ToString())
                            newNode.ToolTipText <- nodeString
                            newNode.BackColor <- Color.Yellow
                            node.Nodes.Add(newNode) |> ignore
                            mapTermToTreeNodeInner (loop.membrane :> Node) 1L newNode
                            mapTermToTreeNodeInner (loop.content :> Node) 1L newNode
                    | :? Sequence as seq -> 
                            let nodeString = seq.ToString(symbols)
                            let newNode = new TreeNode(nodeString + " * " + n.ToString())
                            newNode.ToolTipText <- nodeString
                            newNode.BackColor <- if (t:? LoopingSequence )then Color.Yellow else Color.GreenYellow
                            node.Nodes.Add(newNode) |> ignore
                            for child in Array.ofSeq seq.children do
                                let newNodeInner = new TreeNode(child.ToString(symbols))
                                newNodeInner.ToolTipText <- child.ToString(symbols)
                                newNode.Nodes.Add(newNodeInner) |> ignore
                            done
                    | :? TermVariable as tv -> 
                            let newNode = new TreeNode(tv.ToString(symbols))
                            newNode.ToolTipText <-tv.ToString(symbols)
                            newNode.BackColor <- Color.Red
                            node.Nodes.Add(newNode) |> ignore
                    | _ -> ()
            let nodeString = t.ToString(symbols)
            let root = new TreeNode(nodeString)
            root.Expand()
            root.ToolTipText <- nodeString
            root.BackColor <- Color.LightGreen
            for keyVal in Array.ofSeq t.children do
                mapTermToTreeNodeInner keyVal.Key keyVal.Value root
            done
            root
                

  
        let create<'T> () = 
            let event = Event<'T>()
            event.Trigger, event.Publish

        /// A worker automaton is a reactive automaton running on a dedicated thread of its
        /// own.
        type Worker( step_number:float , time_limit: float(*, graph_refresh_rate:int *), data_refresh_rate:float, fileName : string, useConstant:bool,useIncremental:bool,useUltraM:bool) = class
           // This is a standard signal/queue pair used to implement a message queue
            let signal = new ManualResetEvent(false) 
            let queue = new Queue<_>()
            let mutable simulate_with_step_limit = if step_number > 0.0 then true else false
            let mutable simulate_with_time_limit = not simulate_with_step_limit
            /// These are standard functions to read a queue in a non-blocking fashion.
            let dequeue() = 
                lock (queue) (fun () -> 
                    let msg = queue.Dequeue() in 
                    if queue.Count = 0 then signal.Reset() |> ignore
                    msg)
                    
            let enqueue(msg) = 
                lock (queue) (fun () -> queue.Enqueue(msg)); 
                signal.Set() |> ignore

            let peek ()  =
                let gotOne = signal.WaitOne(0,false) 
                if gotOne then Some(dequeue()) else None 

            let receive () =
                signal.WaitOne(-1,false) |> ignore
                dequeue()
           
            // Capture the synchronization context of the thread that creates this object. This
            // allows us to send messages back to the GUI thread painlessly.
            let callerCtxt = 
                match System.Threading.SynchronizationContext.Current with 
                | null -> null // System.ComponentModel.AsyncOperationManager.SynchronizationContext
                | x -> x
            //do if callerCtxt = null then failwith "Couldn't detect the synchronization context of the calling thread"
            
            let runInCallerCtxt f = 
                match callerCtxt with 
                | null -> 
                    // callerCtxt is null on Mono. This is a bug. System.Threading.SynchronizationContext.Current doesn't return a useful
                    // result. This is a little unfortunate. System.ComponentModel.AsyncOperationManager.SynchronizationContext returns
                    // an inconsistent result.
                    //
                    // So here we works around, where  We find the open form and send to it. 
                    if System.Windows.Forms.Application.OpenForms.Count > 0 then 
                        System.Windows.Forms.Application.OpenForms.Item(0).BeginInvoke(new System.Windows.Forms.MethodInvoker(fun _ -> f())) |> ignore
                | _ -> callerCtxt.Post((fun _ -> f()),null)

            // This events are fired in the synchronization context of the GUI (i.e. the thread
            // that created this object)
            let fireUpdates,onUpdates = create()
            let fireFinishedEarly,onFinishedEarly = create()
            let fireInitGraph, onInitGraph = create()
            let fireError, onError =create()
            let fireTermUpdates, onTermUpdates = create()
            
            let clock = ref(double 0)
            let mutable engine = None
            let mutable model = None
            (*
            //DEBUG PREP3ROCESSING
            
            let (subs, tts) = SCLS.preprocessing.preprocess_rules(model.rules)
            do for sub in subs do Console.WriteLine(sub.ToString())
            do Console.WriteLine("")
            do for tt in tts do Console.WriteLine(tt.ToString())
            do Console.WriteLine("")
            let TT = new SCLS.preprocessing.TransitionTable(tts)
            do Console.WriteLine(TT.ToString())
            let LM = new SCLS.preprocessing.LeavesMatcher(subs)
            //do for a in LM.getMatch((model.term.getLeaves()).Item(0) :?> SCLS.SCLS.Sequence) do
            //    (Console.WriteLine(fst(a).ToString() +" " + snd(a).ToString()))
            //    done
            let AT = new SCLS.preprocessing.AttributeTable(model.term, subs, LM, TT)
            do Console.WriteLine(AT.ToString())
            let m = AT.getMatches() 
            do for mat in m do Console.WriteLine(mat.ToString()) done
            *)
            
            let mutable start_time = System.DateTime.Now
            let mutable stop_time = System.DateTime.Now
            let startTime() = 
                start_time <- System.DateTime.Now
            
            let stopTime() =
                stop_time <- System.DateTime.Now
                Console.WriteLine("Elapsed " + (stop_time - start_time).ToString())
                
            let iterationCounter = ref 0
            
            
            let Init() =
                    //HERE TRY CATCH EXCEPTIONS
                    //try
                        (*
                        // DEBUG POWER SET
                        let a = SCLS.SCLS.Compartment.power_set(new SCLS.SCLS.Compartment())
                        
                        let onechild = new SCLS.SCLS.Compartment()
                        let el = new SCLS.SCLS.Element ResizeArray()
                        let el1 = new SCLS.SCLS.Element(1);
                        let el2 = new SCLS.SCLS.Element(2);                
                        let onlyseq = new SCLS.SCLS.Sequence()
                        onlyseq.AddChild(el1);
                        onlyseq.AddChild(el2);
                        onechild.AddChild(onlyseq,1L);
                        let b = SCLS.SCLS.Compartment.power_set(onechild)
                        
                        let onechildrepeated = new SCLS.SCLS.Compartment()
                        onechildrepeated.AddChild(onlyseq,10L);
                        let c = SCLS.SCLS.Compartment.power_set(onechildrepeated)
                        
                        let twodifferentchild = new SCLS.SCLS.Compartment();
                        twodifferentchild.AddChild(onlyseq,1L);
                        let el3 = new SCLS.SCLS.Element(3);                
                        let secondseq = new SCLS.SCLS.Sequence()
                        secondseq.AddChild(el1);
                        secondseq.AddChild(el2);
                        secondseq.AddChild(el3)
                        twodifferentchild.AddChild(secondseq,2L);
                        let d = SCLS.SCLS.Compartment.power_set(twodifferentchild)
                        *)
                        startTime()
                        if not mono then SplashForm.StartSplash(1, Color.FromArgb(128, 128, 128));  else ()
                        let inputFile = (new System.IO.StreamReader(fileName)).ReadToEnd();
                        model <- Some(SCLS.SCLS_Parser.SCLS_Parser.Parse(fileName))
                        engine <- Some(new Engine_pp.Engine(model.Value,useConstant,useIncremental,useUltraM,false)) // <-- here could use parallel constant search
                        
                        clock := float 0
                        runInCallerCtxt(fun _ -> fireInitGraph(model.Value(*, engine.Value.getStartConcentrations(ref 0.0, true)*), inputFile)  ) 
                        let out = engine.Value.getStartConcentrations(ref 0.0, true)
                        runInCallerCtxt(fun _ -> fireUpdates(out(*, model.term.ToString()*), true, !iterationCounter))
                        // close the splash screen
                        if not mono then SplashForm.CloseSplash(); else ()
                        ()
                    //with
                    //    | e -> (fireError(e.Message)) 
                    
                    
            /// Compute one step (=data refresh rate reactions) of the simulation and call the 
            /// NotifyUpdates callback.  That is, this function provides
            /// glue between the core computation and the computation of that algorithm
            let oneStep(s) = 
                simulate_with_step_limit <- simulate_with_step_limit  && (!iterationCounter + 1 < int step_number)
                simulate_with_time_limit <- simulate_with_time_limit && (!clock < time_limit)
                let simulate = simulate_with_step_limit || simulate_with_time_limit
                match (simulate, simulate_with_step_limit, simulate_with_time_limit) with
                    | (false, _,_) -> (( runInCallerCtxt(fun _ ->  fireFinishedEarly()) ); stopTime();(s, (float (-1))))
                    | (true, true, false) ->
                        /// here do iteration limit
                        /// GET DATA EACH data_refresh_rate iteration
                        for i=1 to int data_refresh_rate 
                            do (
                                                    //iterationCounter := !iterationCounter + 1
                                                    //(Engine.getEnumConcentrations(clock, model,false) |> ignore )
                                                    (engine.Value.getEnumConcentrations(clock,false) |> ignore )
                            )
                        iterationCounter := !iterationCounter + int data_refresh_rate
                        let out = engine.Value.getEnumConcentrations(clock,true)
                        runInCallerCtxt(fun _ -> fireUpdates(out(*, model.term.ToString()*), true, !iterationCounter))
                        //runInCallerCtxt(fun _ -> fireUpdates(out(*, model.term.ToString()*), true, !iterationCounter))
                        // if tau = 0 then there are no match and  there will not be never more match
                        // the simulation is ended
                        if ( fst out = !clock )         then (  (*Console.WriteLine("STOPPED!!")
                                                                Console.WriteLine(engine.Value.AT.ToString() )
                                                                for rm in engine.Value.AT.matches do
                                                                    for m in rm do 
                                                                        Console.WriteLine(m.ToString())
                                                                    done
                                                                done
                                                                
                                                                for m in engine.Value.AT.getMatches() do
                                                                    for mm in m do
                                                                    Console.WriteLine(mm.ToString())
                                                                    done
                                                                    Console.WriteLine("")
                                                                done
                                                                *)
                                                                //Console.WriteLine( )
                                                                stopTime()
                                                                s, float (-1)
                                                                                )
                                                        else (  
                                                                s, (!clock )
                                                                )
                    | (true, false, true) ->
                        /// here do iteration limit
                        /// GET DATA AFTER clock > data_refresh_rate + clock 
                        let limit = !clock + float data_refresh_rate
                        while !clock < limit do
                           iterationCounter := !iterationCounter + 1
                           (engine.Value.getEnumConcentrations(clock,false) |> ignore )
                        done
                        //let ref = graph_refresh_rate
                        //let now = !iterationCounter in 
                        //let rest =  now % ref
                        //let out = Engine.getEnumConcentrations(clock, model, true)
                        let out = engine.Value.getEnumConcentrations(clock,true)
                        //in if rest = 0  then    (runInCallerCtxt(fun _ -> fireUpdates(out(*, model.term.ToString()*), true, !iterationCounter)) ) 
                        //                else    (runInCallerCtxt(fun _ -> fireUpdates(out(*, model.term.ToString()*),false, !iterationCounter)) )
                        runInCallerCtxt(fun _ -> fireUpdates(out(*, model.term.ToString()*), true, !iterationCounter))
                        if ( fst out = !clock )         then (  Console.WriteLine(engine.Value.AT.ToString() )
                                                                //Console.WriteLine( )
                                                                stopTime()
                                                                s, float (-1)
                                                                
                                                                                )
                                                        else (  
                                                                s, (!clock )
                                                                )
                    | _ -> failwith "!"
                
                

               
             
            /// This is the States of the worker's automata using a set of 
            /// tail-calling recursive functions. 
            let rec ResetThen f s = 
                Init()
                f(s)
            
            and Running(s) = 
                match peek() with 
                | None -> StepThen (SleepThen Running) s
                | Some(msg) ->
                    match msg with
                    | Pause -> Paused s
                    | Step -> Running s
                    | Run -> Running s
                    | Reset -> ResetThen Running s
                    | Exit -> Finish s   
          
            and StepThen f s =   
                let s,clock = oneStep(s)
                if not ((clock) = float (-1)) then f s
                else FinishEarly(s)
                
            and SleepThen f s = 
                // A pause between steps of the computation is needed to 
                // give a decent response from the GUI menus. 
                if simulate_with_step_limit 
                    then
                      if data_refresh_rate < 100.0 then Thread.Sleep(int ( 2500.0 / data_refresh_rate)) |> ignore
                     else ()
                      //if data_refresh_rate < 100.0 then Thread.Sleep(int (Float.div 2500.0 data_refresh_rate)) |> ignore
                f(s)      
                
            and Paused(s) = 
                runInCallerCtxt(fun _ -> fireTermUpdates(engine.Value.model.term.ToString(engine.Value.model.symbols), mapTermToTreeNode (model.Value.term) (model.Value.symbols)) )
                match receive() with 
                | Pause -> Paused s
                | Step -> StepThen Paused s
                | Run -> Running s
                | Reset -> ResetThen Paused s
                | Exit -> Finish s 
                
            and FinishEarly(s) =  
                runInCallerCtxt(fun _ -> fireFinishedEarly())
                Paused(s)
                
            and Finish(s) = 
                ()

            /// This is the function that each worker thread ultimately runs 
            let start = ResetThen Running 
            
            
            /// Cleanup our resources.
            let mutable disposed = false
            let cleanup() = 
                if not disposed then 
                    disposed <- true; 
                    signal.Close()
                    
            /// Here is the public API to the worker
            member w.RunAsync () = enqueue(Run)
            member w.StopAsync() = enqueue(Pause)
            member w.ExitAsync() = enqueue(Exit)
            member w.StepAsync() = enqueue(Step)
            member w.ResetAsync() = enqueue(Reset)
            member w.Updates : IEvent<(float * int64 array) (** string*) * bool * int >       = onUpdates
            member w.InitGraph : IEvent<Model *  string>       = onInitGraph
            member w.FinishedEarly = onFinishedEarly
            member w.StartAsync()  = start() |> ignore
            member w.InitAsync () = w.InitGraph |> ignore
            member w.Error : IEvent<string> = onError
            member w.TermUpdates : IEvent<(string * TreeNode)>       = onTermUpdates
              
             
            interface System.IDisposable with 
                member x.Dispose() = cleanup()
            end 
           
            //new () = {}   
            

                
        end
      end