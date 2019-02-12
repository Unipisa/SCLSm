(*
CONSTRUCTION
new(seqs:(Sequence * Int32) array , symbols_count: Int32 , sb:Int32)
/// constuct the nfa 
leaves_matcher = new nfa_leaves_matcher(Array.ofList !leaves_subs,sc,subs.Length) ; 

USAGE
member x.getMatch(input:ResizeArray<Element>) : bindings list array
/// here get the matches througt the nfa
let matches =(x.leaves_matcher.getMatch(seq.children))

*)


#light

namespace SCLS
open System
open System.Collections.Generic
open System.Text
//open Microsoft.FSharp.Math
open SCLS
open Microsoft.FSharp.Core

//open Microsoft.FSharp.Math.BigInt


/// state of nfa leaf matcher (identifier * made binding)
type nfa_state = Int32 * nfa_bindings

/// an entry of transition table of the nfa
type nfa_tt_entry =
        {
            /// start state
            start       :   Int32;
            /// symbol to read for do the transition
            read        :   Element Option
            /// goto state
            goto        :   Int32;
            /// at which variable's symbol had to bind read symbol
            to_bind     :   Int32 Option
        }
    with
        // get string representation
        override x.ToString() : string =
            let read_str = match x.read with Some(i) -> i.ToString() | _ -> ""
            let to_bind_str = match x.to_bind with Some(i) -> i.ToString() | _ -> ""
            "[ " + x.start.ToString() + " - " + read_str + " - " + x.goto.ToString() + " - " + to_bind_str + " ]"
     end  

/// the transition table of the nfa
type nfa_transition_table = nfa_tt_entry array
     
/// an nfa that compute all possible match between a sequence (=leaf) and all the sequence pattern in the set of rules
type nfa_leaves_matcher =
    class
        /// the transition table
        val transition_table : nfa_transition_table
        /// the set of final states (=set of final state * subid)
        val final_states :  (int Set *int) array
        /// the set of final states (=start state * subid)
        val start_states :  (int*int) Set
        /// number of symbols in the alphabet
        val symbols_count : Int32
        /// number of sub pattern (need for create bindings)
        val sub_count  : Int32
        ///
        val sub_seqvar : Int32 Set array
        /// constructor
        /// create and merge the transition table of each sequence
        new(seqs:(Sequence * Int32) array , symbols_count: Int32 , sb:Int32) as this =    
            let a,b,c = nfa_leaves_matcher.make_transition_table(seqs,symbols_count) 
            {transition_table = a; final_states = Set.toArray b; start_states =c; symbols_count = symbols_count; sub_count = sb; sub_seqvar = Array.zeroCreate sb }
            then
                /// here build the array of sequence variable of each sub; if these are bound to None then will bind to Some([])
                for (seq,sub_id) in seqs do
                    let set = ref (Set.empty)
                    for child in seq.children do
                        match child with 
                            | :? SequenceVariable as sv -> set := ((!set).Add(sv.address)) 
                            | _ -> ()
                    done
                    this.sub_seqvar.[sub_id] <- !set
                done
                // DEBUG NFA
                //for aa in a do Console.WriteLine(aa.ToString()) done; Console.WriteLine("");
                
                //for fs in b do
                //        let state_set_str = let s = ref "" in Set.iter (fun(state:Int32) -> s:= !s + " " + state.ToString()) (fst(fs)); !s 
                //        Console.WriteLine(state_set_str + " final for sub_id " + snd(fs).ToString()) 
                //done; Console.WriteLine("");
                //for fs in c do Console.WriteLine(fst(fs).ToString() + " start for sub_id " + snd(fs).ToString()) done; Console.WriteLine("");
                
            /////
                                        
        /// compute the (transition table * (final state * sub_id) for a single sequence pattern
        static member make_tt_single(seq:Sequence, max: Int32, sub_id: Int32) : (nfa_transition_table*(Int32 Set(*final states for*)* Int32(*sub_id*)))  =
            let out = ref []
            let state_counter = ref (max + 1)
            /// here accumulates the variables already met;
            let seen_vars = ref []
            /// here accumulates the state that are the start of some sequence variables automata 
            /// from these states will start the transition that allow to bind to it Epsilon
            let pending_states = ref []
            
            /// given an element and its index in the current sequence, make a transition table entry 
            let makeTransitionTableEntry = fun i (el:Element) ->
                let state,next_state = state_counter := !state_counter + 1
                                       !state_counter - 1,!state_counter
                
                /// create the transitions that allow to bind epsilon to the sequence variables
                let getPendingStatesTransitions(arrival_state,to_read,to_bind) =
                    let out = ref []
                    for ps in !pending_states do
                        out :=
                            {
                            new     nfa_tt_entry
                            with    start       =   ps
                            and     read        =   to_read
                            and     goto        =   arrival_state
                            and     to_bind     =   to_bind
                            }
                            :: !out
                    done
                    !out
                
                /// make the list of transition table entries for the current element of the current sequence
                let tt_entrys = 
                    match el with
                        |   :? SequenceVariable as e -> 
                                (
                                /// if is not the first time that see this variable for this sequence
                                if List.exists (fun (ell:Int32) -> if ell = e.address then true else false) !seen_vars
                                    then
                                        /// compute the transition in which have to read what is bound to variable
                                        [{
                                            new     nfa_tt_entry
                                            with    start       =   state
                                            and     read        =   Some(el)
                                            and     goto        =   next_state
                                            and     to_bind     =   None
                                        }] @ getPendingStatesTransitions(next_state,Some(el),None)
                                    else 
                                        /// else compute two transitions that bind everything reads to the variable
                                        seen_vars := e.address :: !seen_vars
                                        [{
                                            new     nfa_tt_entry
                                            with    start       =   state
                                            and     read        =   None
                                            and     goto        =   next_state
                                            and     to_bind     =   Some(e.address)
                                        };
                                        {
                                            new     nfa_tt_entry
                                            with    start       =   next_state
                                            and     read        =   None
                                            and     goto        =   next_state
                                            and     to_bind     =   Some(e.address)
                                        }
                                        ] @ getPendingStatesTransitions(next_state,None,Some(e.address)) 
                                        |> fun r -> pending_states := state :: !pending_states; r 
                                )
                        |   :? ElementVariable as e ->  
                                (
                                /// if is not the first time that see this variable for this sequence
                                if List.exists (fun (el:Int32) -> if el = e.address then true else false) !seen_vars
                                    then
                                        /// compute the transition in which have to read what is bound to variable
                                        [{
                                            new     nfa_tt_entry
                                            with    start       =   state
                                            and     read        =   Some(el)
                                            and     goto        =   next_state
                                            and     to_bind     =   None
                                        }] @ getPendingStatesTransitions(next_state,Some(el),None)
                                    else 
                                        /// else compute a transition that bind every element reads to the variable 
                                        seen_vars := e.address :: !seen_vars
                                        [{
                                            new     nfa_tt_entry
                                            with    start       =   state
                                            and     read        =   None
                                            and     goto        =   next_state
                                            and     to_bind     =   Some(e.address)
                                        }]   @ getPendingStatesTransitions(next_state,None,Some(e.address))
                                )
                        |   :? ConstantElement as e -> 
                                /// compute the transition that require of read that element
                                (
                                [{
                                    new     nfa_tt_entry
                                    with    start       =   state
                                    and     read        =   Some(el)
                                    and     goto        =   next_state
                                    and     to_bind     =   None
                                }]   @ getPendingStatesTransitions(next_state,Some(el),None) 
                                 )
                        |   _ -> failwith ""
                /// if the current element is constant or element variable (that can not bind to epsilon)
                /// reset the pending_states list; this makes that it is not possible to jump the current element
                if (el :? ConstantElement) || (el :? ElementVariable) 
                    then pending_states := []
                    else ()
                /// add the list of transition table entry generated for 
                out := !out @ tt_entrys  
                      
            /// make the transition table entries for each child in the input sequence
            Seq.iteri makeTransitionTableEntry seq.children
            
            /// make the set of final states
            let final_states =
                if (!pending_states).IsEmpty 
                    then Set.singleton !state_counter
                    else 
                        let epsilon_closure = Set.ofList !pending_states
                        epsilon_closure.Add(!state_counter)
            
            /// return the array of computed transition table entries and the set of final states with the subid of the sequence
            (Array.ofList (!out),(final_states  ,sub_id))
        
        /// merge the transition table computed for each sequence in one transition table
        /// (here to put the logic of share states between different sequences)
        static member make_trie(tts:nfa_transition_table array, finalStates: (int Set*int) Set, startStates: (int*int) Set) 
                        : nfa_transition_table*(int Set*int) Set *(int*int) Set =
            if tts.Length > 0 
                then 
                    let out = ref tts.[0]
                    for i = 1 to tts.Length-1 do
                        out := Array.append !out tts.[i]
                    done
                    !out, finalStates,startStates
                else [||], finalStates, startStates

        /// compute the transition table of an array of sequence with regards sub_id
        static member make_transition_table(seqs: (Sequence*int) array, symbols_count:Int32) : nfa_transition_table*(int Set*int) Set *(int*int) Set =
            let out = Array.zeroCreate (seqs.Length )
            let max_state_counter = ref 0
            let finalStates = ref (Set.empty)
            let startStates = ref (Set.empty)
            nfa_leaves_matcher.make_trie(Array.mapi (fun i e ->  
                                                                startStates := 
                                                                    Set.add (!max_state_counter + 1,snd(seqs.[i])) !startStates 
                                                                let a,(b,c) = 
                                                                    nfa_leaves_matcher.make_tt_single(fst(seqs.[i]), !max_state_counter,snd(seqs.[i]))  
                                                                max_state_counter :=  b.MaximumElement; 
                                                                finalStates := Set.add (b,c) !finalStates; 
                                                                a
                                                    )
                                                    out, 
                                                    !finalStates,
                                                    !startStates)
  
        /// concat the list of element of second bind at the first for each element in binding
        static member concat_nfa_bindings(b1:nfa_bindings, b2:nfa_bindings) = ////FORSE QUI CI VUOLE UN CLONE !! o forse lo fa la  map ??
            Array.mapi (
                        fun i (lo:Int32 list Option) -> 
                        match lo, b2.[i] with 
                            | Some(l), Some(l2) -> Some(l@l2)
                            | None,None -> None 
                            | Some(l), None -> Some(l)
                            | None, Some(l) -> Some(l)
                        ) 
                        b1
        
        /// nfa driver : given an input sequence (in term of Element list = its children) 
        /// make a step for each child of the input sequence 
        member x.getMatch(input:Element array) : bindings list array = 
            /// initialize the starter active states
            let active = ref ( Array.map (fun (s,sid) -> (s, (Array.create x.symbols_count None)) ) (Set.toArray x.start_states) )
            let step_activated = Array.create (input.Length+1) []
            /// here will store the result matches (in i-th position put the binding list of i-th sub's matches)
            let result = Array.create x.sub_count []
   
            /// concat an element to what bound to the i-th identifier in the given binding
            let concatBind(bind:nfa_bindings, e:Element, i:Int32) =
               let newbind = Array.zeroCreate bind.Length
               for j=0 to bind.Length - 1 do if j<>i then newbind.[j] <- bind.[j] done
               match bind.[i] with 
                    | Some(l) -> newbind.[i] <- Some(l @ [e.address])
                    | None -> newbind.[i] <- Some([e.address])
               newbind
            
            /// make one step of nfa 
            let makeStep = fun  i (e:Element) -> 
                // i = index of current read element ; e = current read element
                (
                //for each states activated for this step
                for (state, bind) in !active do
                    //get the transitions that starts with current state
                    let possibleTransition = 
                        Array.filter (fun (tte:nfa_tt_entry) -> if tte.start = state then true else false) x.transition_table
                    //for each possible transition
                    for t in possibleTransition do
                        match t.read with
                            // if had to read something
                            |   Some(el_to_read) -> 
                                    match el_to_read with       
                                        // have to read a constant element         
                                        |   :? ConstantElement as re -> 
                                                // add to next step activated iff what have to read is exactly what is read
                                                if  e.address = re.address    
                                                    then step_activated.[i+1] <- (t.goto, bind) :: (step_activated).[i+1]
                                        // have to read what is already bound to an element variable 
                                        |   :? ElementVariable as re-> 
                                                // add to next step activated iff what have to read is exactly what is bound to the required element variable identifier
                                                 if e.address = (match bind.[re.address] with Some(l) -> List.head l | _ -> failwith "") 
                                                    then step_activated.[i+1] <- (t.goto, bind) :: (step_activated).[i+1]
                                        |   :? SequenceVariable as re ->    
                                                (
                                                /// part of the input that must match with which bound to variable of type sequence at this time
                                                
                                                match (bind.[re.address]) with
                                                    | None -> ()//step_activated.[i+1] <- (t.goto + 1, bind) :: (step_activated).[i+1]
                                                    | Some(bindedTo) ->
                                                        if i + bindedTo.Length <= input.Length 
                                                            then  
                                                                let temp = ( Array.sub input i (bindedTo.Length )  ) 
                                                                let lookahead = Seq.map (fun (el:Element) -> el.address)  temp 
                                                                if List.ofSeq lookahead = (*ResizeArray.ofList*) bindedTo 
                                                                    then step_activated.[i+bindedTo.Length] <- (t.goto, bind) :: (step_activated).[i+bindedTo.Length]
                                                )
                                        |   _ -> failwith ""
                            // if had to read nothing (= everything i read i will bind to what to bind)
                            |   None -> /// concat what read to the t.to_bind identifier in current bind
                                        let to_bind = match t.to_bind with Some(id) -> id | _ -> failwith ""
                                        step_activated.[i+1] <- (t.goto, concatBind(bind, e, to_bind)) :: step_activated.[i+1]
                    done
                done
                /// activate the states for the next step
                active := List.toArray step_activated.[i+1]
                (*//DEBUG : ACTIVE STATES
                Console.WriteLine(e.ToString())
                for (s,b) in !active do 
                    print_string(s.ToString() + " [")
                    for i= 0 to b.Length-1 do
                        match b.[i] with
                            | None -> print_string(i.ToString() + "= None ")
                            | Some(bb) -> for el in bb do
                                            print_string(i.ToString() + "=" + el.ToString() + " ")
                                          done
                    done
                    Console.WriteLine(" ]") 
                done
                *)
                )
            
            /// make a step for each child in input sequence         
            Seq.iteri makeStep input
            
            (*
            //DEBUG : ACTIVE STATES
            Console.WriteLine(input.ToString())
            for (s,b) in !active do 
                print_string(s.ToString() + " [")
                for i= 0 to b.Length-1 do
                    match b.[i] with
                        | None -> print_string(i.ToString() + "= None ")
                        | Some(bb) -> for el in bb do
                                        print_string(i.ToString() + "=" + el.ToString() + " ")
                                      done
                done
                Console.WriteLine(" ]") 
            done
            *)
            
            /// for each active state that is also final for some sub_id store all the possible bindings in the result array
            for (state, bind) in !active do
                match Array.tryPick (fun ((s:Int32 Set),sub_id) -> if s.Contains(state) then Some((sub_id, bind)) else None) x.final_states 
                    with
                        |   None -> ()
                        |   Some((sid, b)) ->   /// bind epsilon to sequence variable of sub that are not bound
                                                for varid in x.sub_seqvar.[sid] do
                                                    match b.[varid] with
                                                        | None -> b.[varid] <- Some([])
                                                        | _ -> ()
                                                done
                                                result.[sid] <- new bindings(b) :: result.[sid] //quì c'era un .Clone sul bind
            done
            /// return the array of binding list that represent the array of matches
            result
    
    end

