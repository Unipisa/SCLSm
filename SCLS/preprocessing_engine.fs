#light

namespace SCLS



//#r @"C:\Program Files\Reference Assemblies\Microsoft\Framework\v3.5\System.Core.dll"
//#R @"C:\Program Files (x86)\Microsoft Parallel Extensions Dec07 CTP\System.Threading.dll"

open System
open System.Threading
open SCLS
open SCLS.preprocessing
open System.Collections.Generic;
open Microsoft.FSharp.Math
open preprocessing;
//open Microsoft.FSharp.Math.BigInt
module Engine_pp = begin
                                   
    let countMatch (lista : Match list)  =
        List.fold (fun accumulator (m:Match) -> accumulator+m.repetitions ) 0L lista

    let countPar_ratefunction ((matchset: Match array array), (rules: Rule array))=
        let attribute_match =
            fun (m:Match) ->
            /// associate at each match the rate function of the rule
            /// computed on the first bindings 
            m,(rules.[m.rule_id]).getRate((List.head m.bind_list)) * float m.repetitions
            
        let attribute_matchlist =
            fun (ml: Match []) ->
            Array.map attribute_match (Array.ofSeq ml)
            
        let attributated_matchset = 
            Array.map attribute_matchlist matchset
            
        let compute_acc_for_attributated_matchlist =
            fun (aml:(Match * float) array) ->
            Array.fold (fun (acc:float) (m:Match,f:float) -> acc + (f  (** float m.repetitions*)  )) 0.0 aml
        
        let m =
            attributated_matchset
            |> Array.map (fun l -> (l,compute_acc_for_attributated_matchlist l))
        let par = Array.fold (fun acc (m,f) -> acc + f) 0.0 m
        m,par 

    let getNthMatchFromAumentedMatchList ((n:float), (augmented_matchlist:((Match * float) array * float)) ) : Match =
        let acc = ref 0.0
        fst(Array.find (fun ((m:Match), (fb:float)) -> (acc := !acc + fb); if n <  !acc  then true else false) (fst(augmented_matchlist)))

    let getRuleNumberFromMatchNumber ((absoluteMatchNumber: float) , (augmented_matchset:((Match * float) array * float) array ) ) :int * float =
        if absoluteMatchNumber < 0.0 then failwith("Error: negative match number (overflow given by rates)")
        let acc = ref 0.0
        let index = Array.findIndex (fun (a,b) -> (acc := !acc + b); if absoluteMatchNumber <= !acc then acc:=!acc-b; true else false) augmented_matchset
        (index, absoluteMatchNumber - !acc)



    let Gillespie ((matchset: Match array array), (rules: Rule array),rnd:System.Random (*System.Random*)) =
        ///DEBUG NUME3RO MATCH PER OGNI REGOLA
        //for rule in rules do Console.WriteLine(rule.left.GetHashCode().ToString()) done
        //for mat in matchset 
        //    do Console.WriteLine(countMatch(List.ofSeq mat).ToString()) done
        //Console.WriteLine("------------------------------------------------")
        ///
        let augmented_matchset,par = countPar_ratefunction(matchset, rules)
        //let par2 = countPar(matchset, rules)
        //Console.WriteLine(par.ToString() + " " + par2.ToString() + " " + (par2=par).ToString())
        let (tau:float) =  if (par = double 0) then double 0 else - log(rnd.NextDouble()) / par
        let (r:float) = rnd.NextDouble() * par //è normale che venga un numero decimale ?
        let ruleNumber,matchNumber = getRuleNumberFromMatchNumber(r,augmented_matchset)
        //Console.WriteLine("Applico regola " + ruleNumber.ToString() + " al suo match " + matchNumber.ToString() + " with tau " + tau.ToString())
        (tau, ruleNumber, matchNumber, augmented_matchset)

    let mergeMatchSet(a:Match array array, b:Match array array) =
            Array.map2 (fun (rs1:array<_>) rs2 -> Array.append rs1 rs2) a b
        
    let removePatternsMatch(a:Match array array, limit:int) : Match array array *  Match array array  =
            let i = ref 0
            Array.partition (fun (m:Match array) -> if !i > limit then i:=!i+1; true else i:=!i+1; false ) a
            
    let countOccurrencesOfPatterns(patternsOccurrences: Match array array, patterns: Rule array) : int64 array =
            let countOccurrences =
                fun (i: int) (mi : Match [])
                    ->
                Seq.fold (fun acc (m:Match) -> acc + (m.repetitions * (int64 ((patterns.[i]).getRate(List.head m.bind_list))) )) 0L mi
                
            Array.mapi countOccurrences patternsOccurrences
            
        


    let getEnumConcentrations((clock: ref<double>), (model:Model), (getConcentrations:bool), (at:AttributeTable), ground_rules: _ array, patterns: _ array, rnd: System.Random, step:bool, useParallelConstantSearch:bool ) : (float *  int64 array)  =
        //Console.WriteLine(model.term.ToString())
        //for keyVal in (model.term).children do
        //    Console.WriteLine(keyVal.Key.ToString())
        //done
        
        let find(what: Compartment, rule_number:int ) =
                let sc = model.symbols.Count()
                let make_match = 
                    ( fun (where,count) ->
                        {
                        new  preprocessing.Match
                        with rule_id = rule_number
                        and  where = where
                        and  bind_list = [new bindings(sc)]  
                        and  repetitions = count
                        and  involved_sons = [|(*where*)|]
                        and  last_binded = [||]
                          }  
                    )
                Seq.map make_match (model.term.find(what))
        
        let getMatchesFromGrounds(grounds: (Rule * int) array) =
            let out = Array.create (model.rules.Length + model.patterns.Length) (Array.zeroCreate 0)
            match useParallelConstantSearch with
                | false -> 
                    for (rule, n) in grounds  do
                        out.[n] <- (Array.ofSeq( find(rule.left, n)))
                    done
                    out
                | true -> failwith "no parellel implementation" 
                    (*
                    Parallel.For(0,grounds.Length - 1,(fun i -> 
                    
                        let rule, n = grounds.[i]
                        out.[n] <- (find(rule.left, n))
                        )
                    )
                    out                 
                    *)
                    (*
                    //use less memory, more cpu, a little slower
                    let task = grounds |> Array.map (fun (rule,n) -> (*out.[n] <-*)async{ return (find(rule.left, n))})
                    let par = Async.Parallel task
                    Async.Run par
                    *)
                    
                   
        (*
        ///PARALLELL VERSION    
        let a = 
            Async.Run(Async.Parallel [ async { -> at.getMatches() };
                                       async { -> getMatchesFromGrounds(ground_rules) } ])
        let (tau, ruleNumber, matchNumber, augmented_matchset) = Gillespie(mergeMatchSet(a.[0],a.[1](*at.getMatches(),getMatchesFromGrounds(ground_rules)*)), model.rules)
        //Console.WriteLine(at.ToString())
        *)
        ///SEQ VERSION
        let (patternsOccurrences: Match [] []) , (matchSet: Match [] [])  = removePatternsMatch(mergeMatchSet((AttributeTable.getMatches(at)) ,getMatchesFromGrounds(ground_rules)),model.rules.Length - 1)
        //let matchSetDirty = at.getMatches()
        
        //let patternsOccurrences, matchSet = removePatternsMatch(matchSetDirty,model.rules.Length -1)
        
        let (tau, ruleNumber, matchNumber, augmented_matchset(*, patternsOccurrences*)) = Gillespie(matchSet, model.rules, rnd)
        let debug = false//true//model.term.getConcentrations(model.symbols.Count()).[1] > 400L//true
        //Console.WriteLine(tau.ToString() + " - " + (Array.length augmented_matchset).ToString() + " - " + (fst(augmented_matchset.[ruleNumber])).Length.ToString() + " - " + step.ToString())
        if(step && tau > 0.0 && Array.length augmented_matchset > 0 && (fst(augmented_matchset.[ruleNumber]).Length > 0) )
                then    
                    let selected_match = getNthMatchFromAumentedMatchList ( matchNumber,(augmented_matchset.[ruleNumber]))
                    
                    //DEBUG ENGINE
                    (*
                    Console.WriteLine("-------------------------------------------------------------------------")
                    Console.WriteLine(model.term.ToString() + " - " + (model.term.GetHashCode()).ToString())
                    let binding_string = List.fold (fun str (b:bindings) -> str + "[" + b.ToString() + " ]") ""  selected_match.bind_list
                    Console.WriteLine("Time " + (!clock).ToString() + " >> Applico la regola " + (((model.rules).[ruleNumber])).name + " al suo " + (int64 matchNumber+1L).ToString() + " match con bind " + binding_string +"\n")
                    *)
                    if debug then
                        Console.WriteLine("----\n"+at.ToString())
                        Console.WriteLine("")
                    
                    let left_hand_side,right_hand_side  = 
                        //if selected_match.bind.isEmpty
                        if  (((model.rules).[ruleNumber])).lhs_var_count = 0
                            then    (((model.rules).[ruleNumber]).left :> Node),(((model.rules).[ruleNumber]).right :> Node) 
                            else    
                                    let bindings_number =  rnd.Next(selected_match.bind_list.Length - 1)
                                    let selected_bind = List.nth selected_match.bind_list bindings_number
                                    match selected_match.last_binded.Length with
                                        | 0 ->
                                            Node.instantiateNode( (((model.rules).[ruleNumber]).left :> Node) ,  selected_bind(*, None*)),
                                            Node.instantiateNode((((model.rules).[ruleNumber]).right :> Node) , selected_bind(*, None*))
                                        | 1 ->
                                            /// here generate a binding for top level ONLY term variable
                                            /// there is one of power set of remaining child taken random
                                            let to_take_a_part = selected_bind.[(selected_match.last_binded.[0])].Value :?> Compartment
                                            let new_lval = new Compartment(None) //OKKIO
                                            for keyVal in to_take_a_part.children do
                                                let r = rnd.Next(0,int keyVal.Value) ////////////APPROX
                                                if r > 0 then new_lval.AddChild(keyVal.Key, int64 r,true)
                                            done
                                            selected_bind.[selected_match.last_binded.[0]] <- Some(new_lval :> Node)
                                            Node.instantiateNode( (((model.rules).[ruleNumber]).left :> Node) ,  List.head selected_match.bind_list(*, None*)), Node.instantiateNode((((model.rules).[ruleNumber]).right :> Node) , selected_bind(*, None*))
                                        | _ -> 
                                            Console.WriteLine("")
                                            failwith "NYI"
                                            let to_share = (selected_bind.[(selected_match.last_binded.[0])].Value).Clone() :?> Compartment
                                            for var in selected_match.last_binded do
                                                ()
                                            done
                                            Node.instantiateNode( (((model.rules).[ruleNumber]).left :> Node) ,  List.head selected_match.bind_list(*, None*)), Node.instantiateNode((((model.rules).[ruleNumber]).right :> Node) , selected_bind(*, None*))
                                                    
                    
                    if debug then    
                        (       
                        //DEBUG INSTANTIATE
                        
                        Console.WriteLine("instanziando LH : " + (model.rules).[ruleNumber].left.ToString() + " with " +  (List.head selected_match.bind_list).ToString() + " obtain " + left_hand_side.ToString())
                        Console.WriteLine(selected_match.ToString() + " - where : " + (selected_match.where).ToString())
                        Console.WriteLine("LH :" + left_hand_side.ToString() + " - " + LanguagePrimitives.GenericHash(left_hand_side).ToString())
                        Console.WriteLine("RH :" + right_hand_side.ToString() + " - " + LanguagePrimitives.GenericHash(right_hand_side).ToString())
                        
                        //Console.WriteLine("before :\n" + at.ToString()) 
                        Console.WriteLine("Term Before :" + model.term.ToString() )
                        //at.update(selected_match(*, (right_hand_side :?> Compartment), (left_hand_side :?> Compartment)*) )
                        )
                    AttributeTable.update(selected_match, (right_hand_side :?> Compartment), (left_hand_side :?> Compartment),at )
                    //Console.WriteLine("AT after remove:\n" + at.ToString())     
                    
                    model.term.Replace(                            
                                        selected_match.where,
                                        left_hand_side,
                                        right_hand_side
                    )
                    //at.update(selected_match, (right_hand_side :?> Compartment), (left_hand_side :?> Compartment) )
                    if debug then
                        //DEBUG REPLACE
                        //Console.WriteLine("AT after remove and replaced term:\n" + at.ToString()) 
                        Console.WriteLine("Term After :" + model.term.ToString() + "\n")
                        //at.computeAttribute(at.root)
                    at.computeAttributeFromRoot();
                        //Console.WriteLine("")
                        ///at.update(selected_match, (right_hand_side :?> Compartment), (left_hand_side :?> Compartment) )
                        //Console.WriteLine("after update:\n" + at.ToString())
                    
        //Console.WriteLine("-----------------------------------------------------------------------------------------------")
        let preReactionClock = !clock
        clock := !clock + tau

        let concentrations = 
            if getConcentrations 
                then 
                    let from_patterns = countOccurrencesOfPatterns(patternsOccurrences, patterns)
                    let from_concentration = if model.exclude.Count = model.symbols.Count() then [||] else model.term.getConcentrations(model.symbols.Count())
                    Array.append  (from_concentration) (from_patterns)//concatenato al numero di match di ogni pattern patternsOccurrences 
                else 
                    null
        (preReactionClock,concentrations)
    (*
    let getStartConcentrations((clock: ref<double>), (model:SCLS.SCLS.Model), (getConcentrations:bool), (at:AttributeTable), ground_rules: _ array, patterns: _ array ) : (float *  int64 array)  =
        let find(what: Compartment, rule_number:int ) =
                let sc = model.symbols.Count()
                let make_match = 
                    ( fun (where,count) ->
                        {
                        new  preprocessing.Match
                        with rule_id = rule_number
                        and  where = where
                        and  bind_list = [new bindings(sc)]  
                        and  repetitions = count
                        and  involved_sons = [|(*where*)|]
                        and  last_binded = [||]
                          }  
                    )
                ResizeArray.map make_match (model.term.find(what))
        
        let getMatchesFromGrounds(grounds: (Rule * int) array) =
            let out = Array.create (model.rules.Length + model.patterns.Length) (new ResizeArray<_>())
            for (rule, n) in grounds  do
                out.[n] <- (find(rule.left, n))
            done
            out

        ///SEQ VERSION
        let patternsOccurrences, matchSet = removePatternsMatch(mergeMatchSet(at.getMatches(),getMatchesFromGrounds(ground_rules)),model.rules.Length -1)

        let from_patterns = countOccurrencesOfPatterns(patternsOccurrences, patterns)
        let from_concentration = if model.exclude.Count = model.symbols.Count() then [||] else model.term.getConcentrations(model.symbols.Count())
        //concatenato al numero di match di ogni pattern patternsOccurrences 

        (0.0,Array.append  (from_concentration) (from_patterns))
     *)
    let makeDependencyGraph(ground_rules: (Rule * int) array, numberOfElements) : int array array = 
        (*
        let out = Array.zeroCreate ground_rules.Length
        //for each reaction i
        for (ir,i) in ground_rules do
        //for each reaction j
            for (jr, j) in ground_rules do
            //if species product i U species reactants j then add j to a.[i]
                
                let temp =(Set.intersectMany [(ir.getProductSet(numberOfElements)); (jr.getReactantsSet(numberOfElements))])
                if (i <> j) && (not(temp.IsEmpty ))
                    then ()
                    else ()    
            done
        
        done
        //return a.[i] <- list of reactions infuenced by the application of reaction i 
        *)
        [||]
     
    type Engine =
        class
            val AT: AttributeTable
            val ground_rules : (Rule * int) array
            val model : Model
            val patterns : Rule array
            val rnd : System.Random 
            val useParallelConstantSearch : bool
            val ground_rules_dependency_graph : int array array
       
            ///PREPROCESSING INIT
            new(model: Model,useConstant:bool,useIncremental:bool,useUltraMemory:bool,useParallel:bool) =
                let patterns_array = 
                    Array.map 
                        (   fun (p:Pattern) -> 
                            let out = new Rule(p.name, p.p, new Compartment(None), p.rate_function_source, model.symbols );
                            out.init(model.symbols); 
                            out 
                         ) 
                         (model.patterns )
                let total_patterns_array = Array.append model.rules patterns_array
                let subs, tts, ground_rules  = preprocess_rules(total_patterns_array,useConstant);
                let TT = new TransitionTable(tts)
                let LM = new LeavesMatcher(subs,  model.symbols.Count());
                let rnd = new System.Random((int)System.DateTime.Now.Ticks) //new System.Random(((int)System.DateTime.Now.Ticks))
                let dependency = makeDependencyGraph(ground_rules, model.symbols.Count())
                
                //DEBUG PREPROCESSING INIT
                let debug = false//true
                if debug then
                    Console.WriteLine(model.symbols.ToString())
                    //for sub in subs do Console.WriteLine(sub.ToString()) done
                    Console.WriteLine(TT.ToString())
                    Console.WriteLine(model.term.ToString())
                    //for r in model.rules do Console.WriteLine(r.ToString()) done
                
                {
                AT = new AttributeTable(model.term, subs, LM, TT, total_patterns_array, model.symbols.Count(), total_patterns_array.Length, useIncremental,useUltraMemory);
                ground_rules = ground_rules;
                model = model;
                patterns = patterns_array;
                rnd = rnd
                useParallelConstantSearch = useParallel
                ground_rules_dependency_graph = Array.zeroCreate (ground_rules.Length)
                }
                then
                System.GC.Collect()
                System.GC.WaitForPendingFinalizers()
                
            member x.getEnumConcentrations(clock,get) =
                getEnumConcentrations(clock, x.model, get, x.AT,x.ground_rules, x.patterns, x.rnd, true, x.useParallelConstantSearch)
                
            member x.getStartConcentrations(clock,get) =
                getEnumConcentrations(clock, x.model, get, x.AT,x.ground_rules, x.patterns, x.rnd, false, x.useParallelConstantSearch)
                //getStartConcentrations(clock, x.model, get, x.AT,x.ground_rules, x.patterns, x.rnd, false)
            
        end



end