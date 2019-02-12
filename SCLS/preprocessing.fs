#light

namespace SCLS
open System
open System.Collections.Generic
open System.Text
open Microsoft.FSharp.Math
//open Microsoft.FSharp.Math.BigInt
open SCLS

//open Microsoft.FSharp.Core.Operators.Checked 

/// this module ....TODO
module preprocessing = begin

    type Operator =
        |   Parallel
        |   Loop
    with
        override x.ToString() : string =
            match x with
                |   Parallel -> "|"
                |   Loop -> "L"
    end
/////////////////PREP3ROCESSING OF RULES HELPER FUNCTIONS DEFINITIONS

    /// Subpattern
    /// represent a subtree of some rule's left hand side
    [<CustomEquality>]
    [<CustomComparison>]
    type Sub_entry =
        {
            /// identifier of the sub pattern
            sub_id      :   int;
            /// sub pattern reference
            sub_content :   ref<Node>;
            /// numbers of the rules of witch the sub pattern is the full rule
            full_match  :   int Set Option;
        }
     with
        /// get string representation
        override x.ToString() : string =
            let full_match_str = 
                match x.full_match with
                    | None -> ""
                    | Some(f) ->  Set.fold (fun   str (b:int) -> str + "[" + b.ToString() + " ]")  "" f
            "[ " + x.sub_id.ToString() + " , " + (!(x.sub_content)).ToString() +
                " , " + full_match_str + " ]"
                
        override x.GetHashCode() = x.sub_id        
        override x.Equals(y:obj) = 
                    match y with 
                        |   :? Sub_entry as se      -> x.sub_id = se.sub_id
                        |   _                       -> false 
        interface System.IComparable with
            member x.CompareTo(y:obj) = 
                match y with
                    |   :? Sub_entry as se          ->  x.sub_id.CompareTo(se.sub_id) 
                    |   _                           ->  -1
        end
        
     end

    /// Transition Table entry
    /// represent one way to compute attributes of a node according to its type and to children's attributes
    type Tt_entry =
        {
            /// operator of node 
            op              :   Operator
            /// required sub of children with their required repetitions number
            children_subs  :   (int*int64) array 
            ///
            term_vars : (int*int64) Set
            /// sub id computed for the node (iff are present the required children's attributes)
            sub_id          :   int
        }
    with
        /// get string representation
        override x.ToString() : string =
            let childSubString = new StringBuilder("[ ")
            for (childSub, repetitions) in x.children_subs do
                childSubString.Append("(" + childSub.ToString() + "*" + repetitions.ToString() + ")") |> ignore
            done
            childSubString.Append(" ]") |> ignore
            let termVarString = new StringBuilder("[ ")
            for set in x.term_vars do
                termVarString.Append("(") |> ignore
                termVarString.Append((fst(set)).ToString() + "*" + (snd(set)).ToString() ) |> ignore
                termVarString.Append(")") |> ignore
            done
            termVarString.Append(" ]") |> ignore
            "[ " + x.op.ToString() + " , " + childSubString.ToString() + " , " + 
                termVarString.ToString() + " , " + x.sub_id.ToString() + " ]"
     end
    
    /// preprocess an array of rules and return the array of sub-patterns and the list of transition table entrys
    let preprocess_rules(rulesArray : Rule array, useConstant: bool) : (Sub_entry array * Tt_entry list * (Rule*int)array) =
        /// -- helper functions definitions
        let sub_counter = ref 0
        let (sub:Sub_entry list ref) = ref List.Empty
        let tt = ref List.Empty

        /// preprocess the single node of the rule; 
        /// it is a recursive function that has to be called on each top-level of rules with fullMatch = true.
        /// They traverse the complete tree of lhs building the regardings sub_entry and tt_entry, stored staticaly out
        /// of the procedure; for add each locl created sub and tt entry call respectivly updateSub and updateTt procedures.
        /// These last adds the computed sub and tt according to some logic of sharing informations between common sub-trees        
        let rec preprocess(current_node : Node, rule_number : int, fullMatch : bool) : unit =
            
            /// add tha local to node producted sub to the already computed list of sub
            /// and return the state in witch the transition will go;
            /// here is the logic of sharing common subtrees informations between different rules
            let updateSub(local_sub) =    (
                let tr_goto = ref (!sub_counter)
                /// filter sub that "share" someting with local sub
                let find_same_content = 
                    (fun (s:Sub_entry) -> 
                    (if     /// same contents
                            (!s.sub_content) = (! local_sub.sub_content)  
                            (*&&
                            /// impose that LoopingSequence differs from Sequence
                            (not(!s.sub_content :? LoopingSequence)  || 
                                ((!s.sub_content :? LoopingSequence)&&(!local_sub.sub_content :? LoopingSequence))
                            
                            ) 
                            *)
                            then    //DEBUG
                            (*
                                    Console.WriteLine((!s.sub_content).ToString()+" with hash "+(!s.sub_content).GetHashCode().ToString() 
                                    + " is the same of " + (!local_sub.sub_content).ToString()  
                                    + " with hash " + (!local_sub.sub_content).GetHashCode().ToString() );
                              *)      
                                    true 
                            else    false ))
                /// try to find the index of the sub that "share" informations with local sub;
                /// the application's logic garanty that at maximum there will be only one
                match(List.tryFindIndex  find_same_content !sub)
                    with
                        | Some(index) ->
                            /// partition the sub list in the list of to mantains subs and the sub that have to be modified;
                            /// toModify contains the sub that have to be modified to adding the local sub informations,
                            /// toMantain contains the list of subs that have to be mantained
                            let  toMantain, toModify = 
                                let a,b = List.partition (fun (s:Sub_entry) -> (if ((!s.sub_content) = (! local_sub.sub_content)) then false else true )) !sub
                                in  a,List.head b
                            
                            let newToInsert =   
                                {
                                new     Sub_entry
                                with    sub_id      =   toModify.sub_id
                                and     sub_content =   ref current_node
                                and     full_match  =   if fullMatch 
                                                            then    match toModify.full_match with
                                                                        |   None -> Some(Set.singleton(rule_number))
                                                                        |   Some(fm) -> Some(Set.add(rule_number) fm) 
                                                            else    toModify.full_match
                                } 
                            sub := toMantain @ [newToInsert]
                            tr_goto := newToInsert.sub_id  
                        | None ->
                            /// if the sub is "fresh" then add to these already computed
                            sub := !sub @ [local_sub]
                            sub_counter := !sub_counter + 1
                /// return return the state in witch the transition will go
                !tr_goto
            )
            
            /// add tha local to node producted tt entry to the already computed list of tt entrys
            /// here is the logic of sharing information on inference of sub between different rules
            let updateTt(local_tt) =    (
                /// compair two sub in base of the type of the operator and the requested children's attributes
                let its_the_same_tt(t, local) =
                    if (not (t.op = local.op)) 
                        then false
                        else (
                            match t.op with
                                |   Parallel -> (local.children_subs = t.children_subs && local.term_vars = t.term_vars)
                                |   Loop -> (local.children_subs = t.children_subs )
                        )
                    
                match(List.tryFindIndex(fun (t:Tt_entry) -> (if its_the_same_tt(t,local_tt) then true else false )) !tt) with
                        | Some(index) ->
                            /// partition the list on transition table entry in the list of to mantains tt and the tt that have to be modified;
                            let toMantain, toModify= 
                                let a,b = List.partition (fun (t:Tt_entry) -> (if its_the_same_tt(t,local_tt)  then false else true )) !tt
                                in  a,List.head b
                            let newToInsert =   
                                {
                                new     Tt_entry
                                with    op          =   local_tt.op
                                and     children_subs = local_tt.children_subs
                                and     term_vars   =   Set.union  (local_tt.term_vars) (toModify.term_vars)
                                and     sub_id      =   local_tt.sub_id
                                } 
                            tt := toMantain @ [newToInsert]  
                        | None ->
                            /// if the tt entry is "fresh" then add to these already computed
                            tt := !tt @ [local_tt]
            )
            /// -- main construction procedure
            /// the contruction progress like a post order visit
            match current_node with
                // -- leaf
                |   :? Sequence as seq -> 
                        ( 
                        /// create the sub entry and add to just computed sub entrys list
                        let local_sub   =    
                            {
                            new     Sub_entry
                            with    sub_id      =   !sub_counter
                            and     sub_content =   ref current_node
                            and     full_match  =   None
                            } 
                        updateSub(local_sub) |> ignore  /// do not need transition's goto
                        )  
                |   :? TermVariable as tv -> ()         /// the term variables are managed by the parent's tt entry
                // -- internal node
                |   :? Compartment  as comp -> 
                        (
                        /// count the number of children that are term variable
                        let number_of_termVar_child (dict:#IDictionary<Node, int64>) =
                            let count = ref 0
                            for keyVal in dict do
                                match keyVal.Key with
                                    |  :? TermVariable -> count := !count + int keyVal.Value //(here approximation!)
                                    | _ -> ()
                            done
                            !count
                            
                        let numberOfVar = number_of_termVar_child comp.children 
                        let childrenubArray = Array.zeroCreate (comp.children.Count  - numberOfVar )
                        let childrenVarArray = Array.zeroCreate (numberOfVar )
                        let sub_iter = ref 0
                        let var_iter = ref 0
                        /// go down recursive
                        for keyVal in comp.children do
                            let child = keyVal.Key
                            let repetitions = keyVal.Value
                            /// call the preprocessing on each children (we will obtain a post order computation)
                            preprocess(child, rule_number, false)
                            if (child :? TermVariable) 
                                then        /// if the child is a term variable then add to the list of request variable
                                            childrenVarArray.[!var_iter] <- (child :?> TermVariable).address,repetitions 
                                            var_iter := !var_iter + 1
                                            
                                else        /// else add its sub to the array of requested children's sub
                                            /// take the last sub of current child (it is the current child sub)
                                            childrenubArray.[!sub_iter] <- ((List.nth  !sub ((List.length !sub)-1)).sub_id,repetitions)
                                            sub_iter := !sub_iter + 1
                        done
                        /// compute local sub pattern according to what computed by children
                        let local_sub   =  
                            {
                            new     Sub_entry
                            with    sub_id      =   !sub_counter
                            and     sub_content =   ref current_node
                            and     full_match  =   if fullMatch 
                                                            then    Some(Set.singleton(rule_number))
                                                            else    None
                            }  
                        /// update the list of sub and get the state in witch the transition will go to
                        let tr_goto = updateSub(local_sub)
                        /// compute local transition table entry  
                        let local_tt    =   
                            {
                            new     Tt_entry
                            with    op          =   Parallel
                            and     children_subs = childrenubArray
                            and     term_vars    =  Set.ofArray childrenVarArray 
                            and     sub_id      =   tr_goto
                            }
                        /// update the list of transition table entrys
                        updateTt(local_tt)
                      )
                |   :? Loop as loop   -> 
                        (
                        /// go down recursive
                        /// call preprocessing on the membrane and get its sub
                        preprocess((loop.membrane :> Node), rule_number, false)
                        let membrane_sub_cnt =  
                            (List.find (fun (s:Sub_entry) -> (if (!s.sub_content) = (loop.membrane :> Node)  then true else false )) !sub).sub_id
                        /// call preprocessing on the content and get its sub
                        preprocess((loop.content :> Node), rule_number,  false)
                        let content_sub_cnt = 
                            (List.find (fun (s:Sub_entry) -> (if (!s.sub_content) = (loop.content :> Node )  then true else false )) !sub).sub_id
                        // calcola sulla base di quanto calcolato sui figli
                        let local_sub   =   
                            {
                            new     Sub_entry
                            with    sub_id      =   !sub_counter
                            and     sub_content =   ref current_node
                            and     full_match  =   if fullMatch 
                                                            then    Some(Set.singleton(rule_number))
                                                            else    None
                            } 
                        /// update the list of sub and get the state in witch the transition will go to   
                        let tr_goto = updateSub(local_sub) 
                        /// compute local transition table entry    
                        let local_tt    =   
                            {
                            new     Tt_entry
                            with    op          =   Loop
                            and     children_subs = if content_sub_cnt = -1 
                                                        then [|(membrane_sub_cnt,1L)|] 
                                                        else [|(membrane_sub_cnt,1L); (content_sub_cnt,1L)|]
                                    /// the loop never require a term variable 
                            and     term_vars  =   Set.empty
                            and     sub_id      =   tr_goto
                            }
                        /// update the list of transition table entrys as the term variables are required at the level of compartment
                        updateTt(local_tt)
                        )
                | _ -> failwith "!"
        
        /// call the preprocessing procedure on the top-level of each rule's lhs
        let ground_rules = new ResizeArray<(Rule * int)>()
        let globalSubCounter = ref 0
        Array.iteri (fun i (rule: Rule) -> ( 
                                            // here split the patterns in constant and variable
                                            if (useConstant)
                                                then
                                                    if rule.lhs_var_count > 0 then preprocess((rule.left :> Node),i,true);
                                                    else (ground_rules.Add((rule ,i)))
                                            
                                            else preprocess((rule.left :> Node),i,true); //if decommented do not split constant patterns
                                            (*//DEBUG TransitionTable contruction
                                            print_string(rule.left.ToString() + "\n")
                                            Console.WriteLine("")
                                            Console.WriteLine(i.ToString() + " ");
                                            for sub_e in !sub do Console.WriteLine(sub_e.ToString()) done
                                            Console.WriteLine("");*)
                                             
                                     ) ) rulesArray
        /// sort the sub list and tranform it in an array
        let sub_res = List.sort  !sub 
                                // (fun (x:Sub_entry) (y:Sub_entry) -> Int32.compare x.sub_id y.sub_id )
                        |> List.toArray
        /// return the array of subs and the list of transition table entry 
        (sub_res, !tt, (Array.ofSeq ground_rules))
        
    /// represent a Match of a rule's left hand side inside the term
    type Match =    
        {
            /// the id of the rule for which the match is relative
            rule_id   :   int  
            /// pointer at the node for which the match it is found
            where     :   Node;
            /// list of bindings that gives that match 
            /// (the differents binding represent many instantiation but only one occurrence of the rule's lhs)
            bind_list      :   bindings list
            /// how many times the rule it is sadisfied in "were" node
            repetitions : int64
            /// array of pointer to the children involved by this match
            involved_sons : (Node (* * int64*)) array
            ///
            last_binded : addr array
        }
     with
        /// get string representation
        override x.ToString() : string =    
            let bind_string = List.fold (fun str (b:bindings) -> str + "[" + b.ToString() + " ]") "" (x.bind_list)
            "Ho "+ (x.repetitions.ToString()) + " of rule number "  
                + x.rule_id.ToString() + " on "  + (x.where).ToString() 
                + " with " + bind_string ; 
     end
     
    /// Attribute of a node (necessary to the match engine)
    type Attribute =    
        {
            /// the node attributated match the sub with this id
            sub_id      :   int
            /// with this array of involved children
            involved_sons : Node array
            /// using one of this binding
            bind_list   :   bindings list
            /// and it is eventually final (a complete match) for this set of rules
            complete_rule_ids : int Set Option
            /// number of times in whic the attributed node sadisfied the sub pattern
            repetitions :   int64
            /// array of address of term variable binded at this node
            last_binded : int array
            /// it is true iff the sub is matched in total (each request sub is satisfied in exact way - there are not child in exceed)
            total : bool
        }
     with
        /// get string representation
        override x.ToString() : string = 
            let is_string = new StringBuilder()
            //for (nodeRef) in x.involved_sons 
            //    do is_string.Append("(" + (!nodeRef).ToString()  + ")") |> ignore
            let bind_string = List.fold (fun str (b:bindings) -> str + "[" + b.ToString() + " ]") "" (x.bind_list) 
            let r_string = new StringBuilder()
            let cr_string =  
                match x.complete_rule_ids with
                    | None -> ""
                    | Some(a) -> Set.fold (fun   str (i:int) -> str + "(" + i.ToString() + ")" )  ""  a     
            "( " + x.sub_id.ToString() + "*" + x.repetitions.ToString() + " - [" + is_string.ToString() + "] - " + 
                bind_string.ToString() + " - " + r_string.ToString() + " - " + cr_string.ToString() +  " )"  
     end
     
    /// represent a possible transition in the computation of node's attribute according to children's attributes
    /// and the possibility of instantiate term variables
    type Transition = 
        class   
        /// requested sub with requested repetitions in the attributes of children in order to do the transition
        val request_subs    :   (int*int64) array
        /// term variables with requested repetitions in order to do the transition
        val request_vars    :   (int*int64) array
        /// identifier of the goto sub pattern : 
        /// the sub-pattern's id sadisfied by a node if its children sadisfy sequested sub and vars 
        val goto             :   int;
        /// constructo
        new(rs,rv,gt) = { request_subs = rs; request_vars = rv; goto = gt;   }
        /// get string representation
        override x.ToString() : string =
            let request_subsString = new StringBuilder("[ ")
            for (childSub, repetitions) in x.request_subs do
                request_subsString.Append("(" + childSub.ToString() + "*" + repetitions.ToString() + ")") |> ignore
            done
            request_subsString.Append(" ]") |> ignore
            let request_varsString = new StringBuilder("[ ")
            for (varId, repetitions) in x.request_vars do
                request_varsString.Append("(" + varId.ToString() + "*" + repetitions.ToString() + ")") |> ignore
            done
            request_varsString.Append(" ]") |> ignore
            "[ " + request_subsString.ToString() + " , " + request_varsString.ToString() + " , " + 
                x.goto.ToString() + " ]"  
     end
    /// the transition table of the algorithm for compute of node's attribute 
    type TransitionTable =    
        class
            /// the transition table
            /// table.[0] -> array of possible transition with Parallel node
            /// table.[1] -> array of possible transition with Loop node
            val table : Transition array array 
            /// constructor
            new(tts:Tt_entry list) as x = 
                /// create the transition table 
                {table = Array.create 2 [||] }
                then
                /// fill each entry of transition table with the given tt entrys 
                for tt in tts do
                    let trans = new Transition(tt.children_subs, Set.toArray tt.term_vars, tt.sub_id)
                    match tt.op with
                        | Operator.Parallel -> x.table.[0] <- ( Array.append [|trans|] (x.table.[0]))
                        | Operator.Loop -> x.table.[1] <- ( Array.append [|trans|] (x.table.[1]))
                done
            /// get the array of transition associated with an operator 
            member x.getTransition(op:Operator) : Transition array =
                match op with
                    | Operator.Parallel -> x.table.[0]
                    | Operator.Loop -> x.table.[1]
        end
     with
        /// get string representation
        override x.ToString() : string = 
            let out = new StringBuilder("<\n\n")
            for i in [0;1] do
                out.Append(match i with | 1 -> (*(Operator.Loop).ToString()+*)  "L : \n"
                                        | _ -> (*(Operator.Parallel).ToString()+*)  "| : \n"
                           ) |> ignore
                for trans in x.table.[i] do
                    out.AppendLine("\t" + trans.ToString()) |> ignore
                done
                out.AppendLine("") |> ignore
            done 
            out.Append(">").ToString()
     end

/////////////////RUNTIME PREP3ROCESSING'S DATA STRUCTURES  
    
    /// A sequence vs leaves sub matcher
    /// it is taken care to find all the possible matches of a (ground) sequence ed all the sequence patterns in the leaf hand sides
    /// of the rules; incampsulate an nfa leaves matcher that, given a sequence, return the set of (sequence) sub id that matches with
    /// the relative list of possible bindings
    type LeavesMatcher =    
        class
            /// an nfa for matching normal Sequences
            val leaves_matcher : nfa_leaves_matcher
            /// an nfa for matching LoopingSequences
            val looping_leaves_matcher : nfa_leaves_matcher
            /// an array of Sequence subs that consist only of a SequenceVariable 
            val seq_var_leaves_subs : (int*addr) array 
            /// an array of LoopingSequence subs that consist only of a SequenceVariable
            /// that cases are dealt a part as thay match against each other Sequence/LoopingSequence with the variable
            /// binded to the full sequence
            val looping_seq_var_leaves_subs : (int*addr) array
            val subs_count : int
            val symbols_count : int
            /// these caches are used for the memorization of matches relative of ground sequences already seen
            /// (this concurs to compute matches by nfa only at the first time that see a sequence )
            val mutable match_cache : Dictionary<Sequence, bindings list array>
            val mutable looping_match_cache : Dictionary<LoopingSequence, bindings list array>
            /// constructor
            new(subs:Sub_entry array, sc:int) = 
                
                    let seq_var_leaves_subs = ref []
                    let loop_seq_var_leaves_subs = ref []
                    let leaves_subs = ref []
                    let looping_leaves_subs = ref []
                    
                    /// get only the id of sub that are sequence and partition in LoopingSequences, LoopingSequences that are only variable
                    /// and Sequences, Sequences that are only variable 
                    Array.filter 
                        (fun sub -> if (!(sub.sub_content) :? Sequence) || (!(sub.sub_content) :? LoopingSequence)  then true else false) subs //solo foglie
                    |> Array.iter
                        (fun sub ->
                            match  !(sub.sub_content) with
                                | :? LoopingSequence as ls -> 
                                    (
                                    let children = (ls:>Sequence).children
                                    if children.Count = 1 
                                        then (  
                                            match children.[0] with 
                                                |   :? SequenceVariable as sv -> 
                                                        loop_seq_var_leaves_subs := (sub.sub_id, sv.address) :: !loop_seq_var_leaves_subs
                                                | _ ->  looping_leaves_subs  := ((ls :> Sequence), sub.sub_id) :: !looping_leaves_subs 
                                             )
                                        else ( looping_leaves_subs  := ((ls :> Sequence), sub.sub_id) :: !looping_leaves_subs )
                                    ) 
                                | :? Sequence as s ->
                                    (
                                    let children = s.children
                                    if children.Count = 1 
                                        then (  
                                            match children.[0] with 
                                                |   :? SequenceVariable as sv -> seq_var_leaves_subs := (sub.sub_id, sv.address) :: !seq_var_leaves_subs
                                                | _ ->  leaves_subs  := (s , sub.sub_id) :: !leaves_subs 
                                             )
                                        else ( leaves_subs  := (s, sub.sub_id) :: !leaves_subs )
                                    )
                                | _ -> failwith ""
                        )
                    {   
                        symbols_count = sc; subs_count = subs.Length;
                        /// create the nfa's
                        leaves_matcher = new nfa_leaves_matcher(Array.ofList !leaves_subs,sc,subs.Length) ; 
                        looping_leaves_matcher = new nfa_leaves_matcher(Array.ofList !looping_leaves_subs,sc,subs.Length); 
                        /// init the arrays of sequence variable sub id
                        seq_var_leaves_subs = Array.ofList !seq_var_leaves_subs 
                        looping_seq_var_leaves_subs = Array.ofList !loop_seq_var_leaves_subs 
                        /// initialise the caches
                        match_cache = new Dictionary<_,_>(HashIdentity.Structural);
                        looping_match_cache = new Dictionary<_,_>(HashIdentity.Structural)   }
                

            /// given a (ground) sequence this return the sub id of subs with which match with relative bindings list                
            member x.getMatch(seq:Sequence) : bindings list array =
                match seq with
                    | :? LoopingSequence as ls -> 
                        (
                        /// check if has already computed the matches for this (ground) looping sequence
                        let ok,res = x.looping_match_cache.TryGetValue(ls)
                        if ok 
                            /// if so, return the stored result  
                            then res
                            else
                                /// here compute the matches of all possible rotations against nfa
                                let res = (
                                    let m = fun (l:Element list) -> x.looping_leaves_matcher.getMatch(Array.ofList l)
                                    /// get the matches given by nfa against each possible rotations 
                                    let partial_res = (Array.map m (ls.allRotations()) )
                                    /// merge the matches of each rotation
                                    let res = Array.create x.subs_count []
                                    for i=0 to x.subs_count - 1 do    //i= subid
                                        for j=0 to partial_res.Length -  1 do //j=number of rotation
                                            res.[i] <- partial_res.[j].[i] @ res.[i]
                                        done
                                    done
                                    res
                                    
                                )
                                /// here bild the matches against the "only sequence variable" looping sequencies 
                                let seq_var_matches = 
                                    let out = Array.create x.subs_count []
                                    for ls_rot in ls.allRotations()
                                        do
                                            for (sub_id, ad) in x.looping_seq_var_leaves_subs do
                                                out.[sub_id] <- 
                                                    let newb = new bindings(x.symbols_count) in 
                                                    let c = new ResizeArray<Element>()
                                                    c.AddRange( ls_rot)
                                                    newb.[ad] <- Some((new LoopingSequence(c,None) :> Node)); 
                                                    [newb]
                                            done
                                        done
                                    out
                                /// marge array of bindings by nfa with the matches against the "only sequence variable" looping sequencies 
                                /// store the result in the cache and return
                                let final_res = Array.map2 (fun a b -> (a @ b)) res seq_var_matches
                                x.looping_match_cache.[ls] <- final_res /// concat each entry 
                                final_res    
                        )
                    | _ -> 
                        /// check if has already computed the matches for this (ground) sequence
                        let ok,res = x.match_cache.TryGetValue(seq)
                        if ok   
                            /// if so, return the stored result
                            then res
                            else
                                /// here bild the matches against the "only sequence variable" sequencies
                                let seq_var_matches = 
                                    let out = Array.create x.subs_count []
                                    for (sub_id, ad) in x.seq_var_leaves_subs do
                                        out.[sub_id] <- 
                                            let newb = new bindings(x.symbols_count) in 
                                            newb.[ad] <- Some((seq :> Node)); [newb] 
                                    done
                                    out
                                /// here get the matches througt the nfa
                                let matches =(x.leaves_matcher.getMatch(Array.ofSeq seq.children))
                                /// merge the two computed match list 
                                let final_res = 
                                    (*if  x.looping_seq_var_leaves_subs.Length > 0 then *)Array.map2 (fun a b -> (a @ b)) matches seq_var_matches// concatenare ogni entrata
                                                                                (*else res*)
                                /// store the result in cache and return
                                x.match_cache.[seq] <- final_res    // WARNING : HERE WE CAN GET MEMORY OVERFLOW.
                                                                    // IT IS NECESSARY TO LIMIT THE SIZE OF THE CACHE
                                final_res
                        
        end
        
    /// check if an instantiation of requested sub on present children it is valid 
    /// and compute the number of times that this instantiation of req sub children satisfy the infered parent's attribute;
    let checkInstantiation(instantiation:(Node ref * int64 * int64)array) : bool * int64 = /////RIVEDERE
        let m = new Dictionary<_,_>()
        
        /// for every present node in the istanziazione it counts the number of times that is selected 
        for (ref, sono, rimangono) in instantiation do
            let present = sono - rimangono
            let ok, res = m.TryGetValue(ref)
            if ok
                then    let (acc,sono2) = res
                        m.[ref] <- (acc + present, sono2)
                else    m.Add(ref,(present,sono))
        done
        
        /// an instantiation is valid iff each node it's selected a number of times smaller 
        /// that the number of times that the node is present; 
        /// the number of times in which the sub inferred from the transition it is satisfied it is computed by 
        /// the multiplication of each requested sub binomial
        let checkIfValidAndComputeRepVal =
            fun (valid:bool ref,prodacc:int64) (keyVal:KeyValuePair<_,_>)  
                ->
            let (_, (acc:int64,sono:int64)) = keyVal.Key, keyVal.Value
            if !valid && acc <= sono    
                then (ref true,prodacc * (*BigInt.to_int64*) int64 (MyMath.Binomial (int sono) (int acc)) )  
                else (ref false,0L)
        
        /// fold on each selected node the computation of validity and 
        /// the number of times in which the sub inferred from the transition it is satisfied
        let valid,repVal = Seq.fold checkIfValidAndComputeRepVal   (ref true, 1L) m
        (!valid, repVal)
    
    let t_s(attr:Attribute, 
            key:Node, 
            vall:int64,
            rep:int64,
            possibleInst:((Node ref * int64 * int64) * bindings list * (int64* int64)) list array,rs:int,i:int) = 
        if (attr.sub_id = rs) && (vall >= rep ) 
            // (posso prendere attr rich quì, con questo bind,(ne voglio tot, ne trovo tot))
            then 
                //Console.WriteLine("soddisfo ! " + attr.sub_id.ToString() + " = " + rs.ToString() + " " + box(key).ToString())
                (possibleInst: _ array).[i]
                    <-
                    List.Cons(
                         ((ref key,vall,vall-rep),attr.bind_list,(rep,vall * attr.repetitions ) ) ,
                         possibleInst.[i]
                    )
    
    
    let snd_of_3 (a,b,c) = b
    let fst_of_3 (a,b,c) = a 
                                                   
    let computeMergedBindingsList(a: (( (Node ref * int64 * int64) * bindings list * (int64 * int64)) array) array) 
        : ((Node*int64*int64) array * bindings list * int64 )array=                                                 
        let out = new ResizeArray<(Node * int64 * int64) array * bindings list * int64>()
        let f(inst : ((Node ref * int64 * int64) * bindings list * (int64 * int64)) array) = 
                let valido, rep_value = checkInstantiation((Array.map (fst_of_3) inst))
                if valido 
                    then
                        //incrocio le liste di bindings degli attributi scelti per ogni figlio coinvolto
                        match bindings.mergeArrayOfBindingsList(Array.map (snd_of_3) inst) with
                            |   None        ->  ()
                            |   Some(bl)    ->  ( out.Add(((Array.map (fun ((aa,bb,cc),b,c) -> (!aa,bb,cc)) inst),bl,rep_value)) )
                        else ()
             
        Array.iter f a
        (Array.ofSeq out)
                                                    
    let crossIntantiations(pi : ((Node ref*int64*int64) * bindings list * (int64*int64) ) array array)
        : ((Node ref*int64*int64) * bindings list * (int64*int64) ) array array
                                                              =
        /// produttoria delle dimensioni di ogni lista di possible intantiations
        let z = Array.fold (fun acc (lista: _ array) -> acc * lista.Length) 1 pi
        /// creo un array grando quante sono tutte le possibili combinazioni
        /// di come prendere ogni sub richiesto da ogni modo possibile di prenderlo tra i figli
        let t = Array.zeroCreate z
        for i=0 to t.Length - 1 do t.[i]<- new ResizeArray<_>() 
        let x = ref z
        //per ogni sub richiesto
        for i = 0 to pi.Length - 1 do
            // numero di figli trovati per il sub corrente
            let current_length = Array.length pi.[i] 
            /// quante volte devo ripetere ogni scelta del figlio, del sub i-esimo in t
            /// prima di spostarmi all'elemento successivo fino ad aver riempito tutto t
            x := !x / (current_length)
            // per ogni riga di t
            //for s = 1 to pi.[i].Length-1  do 
            let iter = ref 0
            for poss in pi.[i] do
                for p = 0 to !x-1  do
                        t.[!iter].Add(poss);
                        iter := !iter + 1
                done
            done
        done
        t |> Array.map (fun l -> Array.ofSeq l)

    /// Hash table of the attributes of term
    type AttributeTable =    
        class
            /// hash table on node ref with the list of associated attributes
            val table : Dictionary<Node, Ref<ResizeArray<Attribute>> >
            val cache_table : Dictionary<Node, Ref<ResizeArray<Attribute>>>
            val counter_table : Dictionary<Node, int>
            val subs    : Sub_entry array
            val leaves   : LeavesMatcher
            val tt      : TransitionTable
            /// root of the term
            val root    : Compartment
            val rules   : Rule array
            val rule_count : int
            val symbols_count : int       
            val mutable matches :  Match ResizeArray array  
            val useIncremental : bool
            val useUltraMemory :bool
            val max_size : int
            /// constructor
            new(term: Compartment, s: Sub_entry array, l: LeavesMatcher, t: TransitionTable, rs : Rule array, sc: int, rc: int, useI:bool, useUltraMemory:bool) as x = 
                {
                table =  new Dictionary<Node, Ref<ResizeArray<Attribute>>>(HashIdentity.Reference)
                cache_table =  new Dictionary<Node, Ref<ResizeArray<Attribute>> >(HashIdentity.Structural)
                counter_table =  new Dictionary<Node, int >(HashIdentity.Structural)
                root = term; subs = s; leaves = l; tt = t;
                rule_count = rc(*rs.Length*); 
                matches = Array.zeroCreate rs.Length 
                symbols_count = sc; rules = rs
                useIncremental = useI
                useUltraMemory = useUltraMemory
                max_size = 1000
                }
                then 
                AttributeTable.clearMatches(x)
                AttributeTable.computeAttribute((term :> Node),x)
          
            static member compute(node:Node,x:AttributeTable) =
                /// initialize the list of attribute of the node
                let attr_list = new ResizeArray<Attribute>()
                /// compute the attribute list regards to the type of the node
                match node with
                    |   :? Sequence as sequence -> 
                            (     
                            /// ask to the leaves matcher the list of subpattern
                            /// against witch the sequence match
                            /// with relative bindings        
                            x.leaves.getMatch(sequence)
                            |>
                            /// for each match create an attribute
                            Array.iteri 
                                (fun i l ->       
                                    if not l.IsEmpty  
                                        then
                                            let attrib = 
                                                {
                                                new Attribute
                                                with    sub_id = i  
                                                and     involved_sons = [|(sequence :> Node)|]
                                                and     bind_list = l
                                                and     complete_rule_ids = (x.subs.[i]).full_match                                
                                                and     repetitions = 1L
                                                and     last_binded = [||]
                                                and     total = true
                                                } 
                                            /// add the attribute to the attribute table                               
                                            attr_list.Add(attrib)
                               )                                
                            )
                    |   :? Loop as loop    -> 
                            (
                            /// first compute recursive the attribute of the membrane and of the content
                            AttributeTable.computeAttribute((loop.membrane :> Node),x)
                            AttributeTable.computeAttribute((loop.content :> Node),x)
                           /// for each per possible transition for Loop, compute the attributes for current node
                            for tr in x.tt.getTransition(Operator.Loop) 
                                do AttributeTable.computeAttributeForLoopTransition(tr,loop,attr_list,(x.table.[(loop.membrane :> Node)]),(x.table.[(loop.content :> Node)]), x) 
                            )
                    |   :? Compartment as comp -> 
                            (
                            let child_count = comp.childCount
                            /// first compute the attributes for each children
                            for keyVal in Array.ofSeq comp.children do 
                                AttributeTable.computeAttribute(keyVal.Key,x) done ///////// ???VIENE MODIFICATA FORSE DALLA CREAZIONE DEL BINDING PER LA VAR
                            // calcolo gli attributi dati da ogni possibile transizione
                            for tr in x.tt.getTransition(Operator.Parallel) do 
                                AttributeTable.computeAttributeForCompartmentTransition(tr,comp,attr_list,x) done
                            )
                    |   _ -> ()

                ref (attr_list )        
                   
            member x.computeAttributeFromRoot() :  unit  = 
                AttributeTable.computeAttribute(x.root,x) 
            
            /// compute the attributes for the nodes of term
            /// take 2 parameter : the node from start to, the parent of the node if any
            /// to compute for entire tree call with (root)
            static member computeAttribute(node : Node,x:AttributeTable) :  unit  =
                if x.table.ContainsKey(node) 
                then () 
                else
                    let ok2,res2 = x.cache_table.TryGetValue(node)
                    if ok2 
                        then (
                            x.table.[node] <- res2
                            AttributeTable.increment(node,x)////??/////
                            )
                        else (
                            let res3 = AttributeTable.compute(node,x)
                            x.table.[node] <- res3
                            AttributeTable.add(node(*.Clone()*),res3,x) //UNCLONE
                            )
               
                match node with
                                | :? Compartment as comp -> 
                                        for keyVal in Array.ofSeq comp.children do
                                            AttributeTable.computeAttribute(keyVal.Key,x)
                                        done
                                | :? Loop as loop ->
                                        AttributeTable.computeAttribute(loop.membrane,x)
                                        AttributeTable.computeAttribute(loop.content,x)
                                | _ -> ()
            
            ////METODI PER GESTIONE MATCH
            
            static member makeMatch(rule_match,attrib,node,x:AttributeTable) =
                /// se la regola è annullabile segnalo match solo se la bindings list non è vuota
                /// e in tal caso filtro solo i bindings non nulli
                
                let toSignal = ref true
                let bindingsList = ref []
                
                // check if the rule can be instantiated to Epsilon
                if x.rules.[rule_match].voidable 
                    then
                        /// if the rule is voidable then signal match iff exist a not empty binding
                        /// and if exist filter out the empty bindings
                        if attrib.bind_list.Length = 0 
                                then toSignal := false
                                else bindingsList := List.filter (fun (b:bindings) -> not b.isEmpty) attrib.bind_list
                                     toSignal := not bindingsList.Value.IsEmpty
                    else     bindingsList := attrib.bind_list
                
                if !toSignal
                    then
                        let currentMatch = 
                            if (((node:Node) :? Compartment) && attrib.last_binded.Length > 0 ) 
                                then
                                        {
                                            new Match
                                            with rule_id = rule_match
                                            and  where = node
                                            and  bind_list = !bindingsList
                                            and  repetitions = attrib.repetitions
                                            and  involved_sons = attrib.involved_sons
                                            and  last_binded =  attrib.last_binded
                                        }
                                else
                                        {
                                            new Match
                                            with rule_id = rule_match
                                            and  where = node 
                                            and  bind_list = !bindingsList
                                            and  repetitions = attrib.repetitions
                                            and  involved_sons = attrib.involved_sons
                                            and  last_binded = [||]
                                        }
                        (x.matches.[rule_match]).Add(currentMatch)
                        
            static member signalMatches(node,attrib, x:AttributeTable) =
                for a in attrib.complete_rule_ids.Value do AttributeTable.makeMatch(a,attrib,node,x) done
                            
                  
            static member getMatches(x:AttributeTable) : Match array array  =  
                AttributeTable.clearMatches(x)    
                for keyVal in x.table do
                    for attrib in (!keyVal.Value) do
                        if attrib.complete_rule_ids.IsSome
                            then for a in attrib.complete_rule_ids.Value do AttributeTable.makeMatch(a,attrib,keyVal.Key,x) done
                            else ()
                    done
                done
                let toArray (a:ResizeArray<_> array): _ array array = 
                    Array.map (fun (el:ResizeArray<_>)-> Array.ofSeq el ) a

                x.matches |> toArray
                
            static member RemoveUp(n:Node,x:AttributeTable) =
                    let mutable current = n
                    AttributeTable.remove(current,x)
                    while current.parent.IsSome  do
                        current <- current.parent.Value 
                        AttributeTable.remove(current,x)
                    done 

            static member RemoveDown(n:Node,x:AttributeTable) =
                    match n with
                        | :? Compartment as comp -> 
                                AttributeTable.remove(comp,x)
                                for keyVal in comp.children do
                                    AttributeTable.RemoveDown(keyVal.Key,x)
                                    AttributeTable.remove(keyVal.Key,x)
                                done
                        | :? Loop as loop ->
                               AttributeTable.RemoveDown((loop.content :> Node),x) 
                               AttributeTable.remove(loop.membrane,x)
                               AttributeTable.remove(loop.content,x)
                               AttributeTable.remove(loop,x)
                        | _->(AttributeTable.remove(n,x))
            
            static member update(m: Match, left:Compartment, right:Compartment, x:AttributeTable) = 
                if x.useIncremental then    
                    AttributeTable.remove(m.where,x)
                    AttributeTable.RemoveUp((m.where),x)                        ///here we can exploite more memory
                    let temp =ref(Set.ofSeq m.involved_sons)
                    for keyVal in right.children do
                        temp := temp.Value.Add(keyVal.Key )
                        
                    done
                    for n in !temp do AttributeTable.RemoveDown(n,x)
                    x.counter_table.Clear()
                    x.table.Clear()       
                else 
                    x.cache_table.Clear()
                    x.table.Clear()  
                    x.counter_table.Clear()
                    
            
            static member clearMatches(x:AttributeTable) = 
                for i=0 to x.matches.Length - 1 do
                    x.matches.[i] <- new ResizeArray<_>()
                done
            
            static member remove(node:Node,x:AttributeTable) =
                let ok2,res2 = x.counter_table.TryGetValue(node)
                if ok2 
                    then (
                        if res2 > 1 
                            then x.counter_table.[node]<- res2-1;
                            else x.counter_table.[node]<- res2-1;
                                 if x.useUltraMemory then if not (node :? Sequence ) then x.cache_table.Remove(node) |> ignore else ()
                                                     else x.cache_table.Remove(node) |> ignore
                        )
                    else ()
              
            
            static member increment(node,x:AttributeTable) =
                let ok2,res2 = x.counter_table.TryGetValue(node)
                if ok2 
                    then (x.counter_table.[node]<- res2+1;  )
                    else (x.counter_table.[node]<- 1; )
            
            
            static member add(node,res,x:AttributeTable) = 
                x.cache_table.Add(node, res)
                x.counter_table.[node]<- 1;
            
            member x.remove_some() =
                let rec remove n =
                    if n > 0 
                    then    let someNotUsed = Seq.tryPick(fun (keyVal: KeyValuePair<_,_>) -> if x.counter_table.[keyVal.Key] <= 0 then Some(keyVal.Key) else None ) x.cache_table
                            if  someNotUsed .IsSome         then x.cache_table.Remove( someNotUsed .Value)  |> ignore; remove (n-1) 
                                                            else ()
                remove (x.max_size / 10)
                
           
                
            override x.ToString() : string = 
                let out = new StringBuilder("Table\n")
                for keyVal in x.table do
                    let attr_list = keyVal.Value                                     
                    out.AppendLine(keyVal.Key.ToString() + ": " + keyVal.Key.GetType().ToString() + " - "  + keyVal.Key.GetHashCode().ToString()) |> ignore
                    for attr in !attr_list do
                        out.AppendLine("\t"+attr.ToString()) |> ignore
                    done
                 
                    
                done
                
                out.Append("Cache_Table\n") |> ignore
                for keyVal in x.cache_table do
                    out.AppendLine(keyVal.Key.ToString() + ": " + keyVal.Key.GetType().ToString() + " - "  + keyVal.Key.GetHashCode().ToString()) |> ignore
                    for attr in !((keyVal.Value)) do
                        out.AppendLine("\t"+attr.ToString()) |> ignore
                    done
                out.ToString()
            
            ////////////////////METODI PER CALCOLO ATTRIBUTI
            static member findReqSubInChildsAttr(i,((rs, rep)), possibleInst, x:AttributeTable, comp:Compartment) = 
                //per ogni figlio
                for keyVal in comp.children do
                    
                    // per ogni attributo del figlio
                    let ok,res = x.table.TryGetValue(keyVal.Key)
                    if ok
                        then    for attr in !res do t_s(attr,keyVal.Key,keyVal.Value,rep,possibleInst,rs,i) done 
                        else    ()
                done
                
            static member computeAttributeForLoopTransition(tr:Transition, loop:Loop, attr_list, membrane_attr, content_attr, x:AttributeTable) =
                if (tr.request_subs.Length > 1) 
                    then    /// case in witch there are not requested variable in the transition 
                            /// (always : the loop can not contains term variable directly)
                        let membrane_request_sub =  fst(tr.request_subs.[0]) 
                        let content_request_sub =   fst(tr.request_subs.[1])
                        /// filter on the attributes of membrane and content to get only witch have requested sub 
                        /// and for for which the sub pattern is sadisfied in total (no node in exceed)  
                        let possible_membrane_inst = 
                            Seq.filter (fun (attr) -> ( attr.total && attr.sub_id =  membrane_request_sub  ) ) (!membrane_attr)
                        let possible_content_inst = 
                            Seq.filter (fun (attr) -> ( attr.total && attr.sub_id = content_request_sub  ) ) (!content_attr)
                        if
                            /// if there are almost one possible way to do the transition 
                            not (Seq.isEmpty (possible_membrane_inst)) &&   not (Seq.isEmpty (possible_content_inst)) 
                        then 
                            // x.table.[ref node] <-+ tr.goto
                            (
                            //let possible_inst = ref []
                            /// for each way of getting the requested sub between children's attribute
                            for membr_attr in possible_membrane_inst do
                                for cont_attr in possible_content_inst do
                                    let current_bindings_list = bindings.mergeBindingsList(membr_attr.bind_list, cont_attr.bind_list )
                                    /// if the binding do not clash add to the list of possible instantiations of req sub between children's attrib
                                    match current_bindings_list with 
                                        | None -> ()
                                        | Some(bl) ->
                                            (
                                                if bl.Length > 0 
                                                    then
                                                        let attrib = 
                                                            {
                                                            new Attribute
                                                            with    sub_id = tr.goto  
                                                            and     involved_sons = [|((loop.membrane :> Node));((loop.content :> Node)) |]
                                                            and     bind_list =  bl
                                                            and     complete_rule_ids = (x.subs.[tr.goto]).full_match 
                                                            and     repetitions = cont_attr.repetitions // * m_attr.repetitions=1
                                                            and     last_binded = [||]//membr_attr.last_binded//[||]
                                                            and     total = true
                                                            }
                                                        in
                                                        attr_list.Add(attrib)
                                            )
                                done                    
                            done
                            )
                    else () /// case in whitch have variables in loop transition : never (the loop can not contains term variables directly
  
            static member getPossibleInstantiations(tr: Transition, comp:Compartment, x:AttributeTable) 
                : ((Node *int64(*quanti ne prendo*)*int64(*quanti ne rimangono*)) array * bindings list * int64) array =
               
                /// dato il vettore di figli richiesti dalla transizione corrente
                /// restituisce l'array di possibili instanziazioni con relativo binding per ogni sub richiesto
                let numberOfReqSubs = tr.request_subs.Length
                // array grande quanti sono i sub richiesti per la transizione; conterrà la lista (node ref * bindings) di come può essere scelto ogni sub richiesto tra i figli
                let possibleInst = Array.create numberOfReqSubs []
                if numberOfReqSubs > 0
                    // calcolo tutti i modi di prendere i sub richiesti tra i figli del nodo
                    then (
                        
                        // per ogni sub richiesto
                        for i=0 to numberOfReqSubs -  1 do AttributeTable.findReqSubInChildsAttr(i,tr.request_subs.[i],possibleInst,x,comp)
                    )
                /// ora in possibleInst ho il vettore di possibili modi di prendere (node ref, bindings) ogni sub richiesto
                /// tra i figli
                if (Array.exists ((=) []) possibleInst)
                    ///qualche sub richiesto nn ha nemmeno un modo di essere preso allora non ho 
                    then ([||]) 
                    else    ///costruisco la serie di possibili instanziazioni del sub sui figli
                        (
                        let accetable_subs_instantiations_array = crossIntantiations(Array.map (fun (ra: _ list) -> (Array.ofList ra )) possibleInst) 
                        let result = computeMergedBindingsList(accetable_subs_instantiations_array) 
                        result
                                  )    

            /// compute the list of attribute of current node for a Parallel transition
            static member computeAttributeForCompartmentTransition(tr:Transition,comp:Compartment,attr_list,x:AttributeTable) =    
                // array grande quanti sono i sub richiesti per la transizione; conterrà il numero di ripetizioni richieste per ogni sub
                let request_subs_count =
                    tr.request_subs
                    |> Array.fold
                        (fun acc (sub_id,rep) -> acc + rep
                        ) 0L
                let total_transition = ref false
                let can_do_transition = ref true
                match request_subs_count - comp.childCount with
                    |   0L -> total_transition := true
                    |   n ->  if n > 0L 
                                then (can_do_transition := false ) /// la transizione non è applicabile
                                else (total_transition := false  )
                if !can_do_transition 
                    then
                    (
                    match tr.request_subs.Length with
                        | 0 -> /// need only a variable
                             (
                                match tr.request_vars.Length with  
                                    | 1 ->                   
                                        //Console.WriteLine(comp.ToString())                    
                                        let remaining_child = comp.children
                                        let involved_by_bind = Array.map (fun (keyVal:KeyValuePair<_,_>) -> keyVal.Key) (Array.ofSeq remaining_child)
                                        //costruzione dell'attributo con i figli rimanenti legati alla variabile
                                        let newbind = new bindings(x.symbols_count)
                                        let requestVariable = fst(tr.request_vars.[0]) ////////////ANDRA RIVISTO CON PIù VARIABILI PER COMPARTMENT: vanno redistribuiti i figli in modo casuale
                                        newbind.[requestVariable] <- Some((*node*)(new Compartment(remaining_child, None) :> Node))//DA RIVEDERE!!
                                        let attrib = 
                                            {
                                            new Attribute
                                            with    sub_id = tr.goto  
                                            and     involved_sons = involved_by_bind
                                            and     bind_list =  [newbind]
                                            and     complete_rule_ids = (x.subs.[tr.goto]).full_match 
                                            and     repetitions = 1L 
                                            and     last_binded = if (newbind.[requestVariable]).IsSome then [|requestVariable|] else [||]
                                            and     total = true
                                            }
                                        attr_list.Add(attrib)
                                      | _ -> failwith "NYI"
                                  
                                )
                        | n -> /// need some child's attribute 
                            (
                            // calcolo tutti i modi di prendere i sub richiesti tra i figli del nodo
                            ///let possibleInstantiations = 
                            // e per ognuno aggiungo l'attributo dato dalla transizione alla lista di attributi del nodo corrente
                            // x.table.[ref node] <-+ tr.goto
                            let possibleInsts:((Node *int64(*quanti ne prendo*)*int64(*quanti ne rimangono*)) array * bindings list * int64) array 
                                = AttributeTable.getPossibleInstantiations(tr,comp,x)
                            
                           (*
                            // DEBUG POSSIBLE INSTANTIATIONS 
                            Console.WriteLine("\n" + tr.ToString())
                            let i = ref 0
                            for instt,bo,repVal in possibleInsts do
                                    Console.WriteLine((!i).ToString())
                                    i := !i + 1
                                    for (chi,prendo,lascio) in instt do
                                        Console.WriteLine("- " + chi.ToString() + " " + prendo.ToString() + " " + lascio.ToString())
                                    done
                                    for b in bo do
                                        Console.WriteLine(b.ToString())
                                    done   
                            done
                            *)
                            
                            if possibleInsts.Length > 0 then 
                                match tr.request_vars.Length with
                                    | 0 ->
                                    (
                                    for inst,bo,repVal in possibleInsts do
                                        //costruzione dell'attributo senza legare variabili
                                        let attrib = 
                                            {
                                            new Attribute
                                            with    sub_id = tr.goto  
                                            and     involved_sons =(Array.map (fun (a,b,c) -> (a)) inst )
                                            and     bind_list = bo
                                            and     complete_rule_ids = (x.subs.[tr.goto]).full_match
                                            and     repetitions = repVal
                                            and     last_binded = [||]
                                            and     total = !total_transition
                                            }
                                        attr_list.Add(attrib)
                                    done
                                    )
                                    | 1 ->
                                        (
                                        
                                        for inst,bo,repVal in possibleInsts do
                                            // lega l'unica variabile a i figli rimanenti 
                                            //se ci sono richieste variabili e il figlio che mi fà soddisfare la transizione ha un numero di ripetizioni maggiori di quelle richieste
                                            // allora devo considerare la differenza come figli rimanenti 
                                            // segnalo 1 solo match con tutti i possibili binding che sono 2+n dove n è la somm delle ripetizioni di ogni figlio rimanente
                                            //in pratica tengo sempre binding normali ma li considero come insiemi di parti quando ho sub finali (compart) che hanno variabili
                                
                                            ///////////////////////////////////////////////////////////////////BUG///////////////////////////////////////////////////////////////
                                            //nel terzo campo di inst.[0] cè il numero di volte che rimane  
                                            let remaining_child= (
                                                let rc = new Compartment(comp.parent)
                                                for keyVal in comp.children do
                                                    //let parent = keyVal.Key.parent
                                                    // try to find 
                                                    let res = Array.tryFind (fun (nr:Node ,taken:int64,remained:int64) -> LanguagePrimitives.PhysicalEquality (nr) keyVal.Key(*nr = ref keyVal.Key*)) inst //non trova cosa deve trovare
                                                    match res with 
                                                        | None ->   
                                                                    rc.AddChild(keyVal.Key.Clone() , keyVal.Value,true) //UNCLONE                                                                                      
                                                        | Some((r,t,rem)) ->  if rem > 0L then rc.AddChild(keyVal.Key.Clone() , rem ,true) 
                                                done
                                                rc
                                                )
                                            if remaining_child.childCount > 0L then //review 16-5 //NON PERMETTO DI LEGARE VARIABILI TERMINE DENTRO I LOOP A NULLA
                                                let involved_by_bind = (Seq.map (fun (keyVal:KeyValuePair<_,_>) -> keyVal.Key) (remaining_child.children)) |> Array.ofSeq
                                                //costruzione dell'attributo con i figli rimanenti legati alla variabile
                                                let newbind = new bindings(x.symbols_count)
                                                let requestVariable = fst(tr.request_vars.[0])
                                                newbind.[requestVariable] <- if remaining_child.children.Count > 0 
                                                                                    then Some(remaining_child:>Node)//Some((new Compartment(remaining_child,None) :> Node)
                                                                                    else None
                                            
                                                match bindings.mergeBindingsList(bo,[newbind]) with
                                                    | None -> ()
                                                    | Some(bl) -> 
                                                        let attrib = 
                                                            {
                                                            new Attribute
                                                            with    sub_id = tr.goto  
                                                            and     involved_sons = Array.append (Array.map fst_of_3 inst ) involved_by_bind 
                                                            and     bind_list =  bl
                                                            and     complete_rule_ids = (x.subs.[tr.goto]).full_match
                                                            and     repetitions = repVal
                                                            and     last_binded = if (newbind.[requestVariable]).IsSome then [|requestVariable|] else [||]
                                                            and     total = true
                                                            }
                                                        attr_list.Add(attrib)
                                        done 
                                        )
                                    | n -> failwith "NYI"
                                
                                )
                            )//end then (if reqsub.length > 0 )                            
     end
end //end of preprocessing module