#light
#nowarn "62"

namespace SCLS


open System
open System.Collections.Generic;
open System.Text;
open Microsoft.FSharp.Math
open SCLS
//open Microsoft.FSharp.Math.BigInt

(*
#r @"..\CodeExpressionEvaluator\bin\Debug\CodeExpressionEvaluator.dll";;
#r @"CodeExpressionEvaluator.dll";;
*)
//open SCLS




module MyMath =
    begin
    
    type ITable<'a, 'b> =
      inherit System.IDisposable
      abstract Item : 'a -> 'b with get
      
    let memoize f (name:string) =
          //let LIMIT = 100000000
          let outerTable = new Dictionary<_,_>()
          //let outerTable = new Dictionary<_,_>(HashIdentity.Structural)  //uguale
          //let pos_count = ref 0
          //let  neg_count = ref 0
          { new ITable<_,_> with
              member t.Item
                with get(n) = 
                  if outerTable.ContainsKey(n) 
                  then 
                    //pos_count := !pos_count +  1
                    //System.Console.WriteLine(name + " + " + (!pos_count).ToString() + "   ---" + (!pos_count).ToString() + "-" + (!neg_count).ToString() +  "----" + n.ToString());
                    outerTable.[n]
                  else
                    //neg_count := !neg_count +  1
                    //System.Console.WriteLine(name + "- " + (!neg_count).ToString() + "   -- " + (!pos_count).ToString() + "-" + (!neg_count).ToString() +  "----" + n.ToString());
                    let res = f n
                    //if outerTable.Count > LIMIT then t.Dispose()
                    outerTable.Add(n, res)
                    res

              member t.Dispose() =
                //System.Console.WriteLine(name + " " + (!pos_count).ToString() + "-" + (!neg_count).ToString() );
                    
                outerTable.Clear()
          }
          
    let memoize_old f =  (*fun x -> f x*)
        let cache = Dictionary<_, _>()
        fun x ->
            if cache.ContainsKey(x) 
            then System.Console.WriteLine("Buffered" + x.ToString());cache.[x]
            else let res = f x
                 cache.[x] <- res
                 res
    
    /// The constant pi = 3.141596...
    let PI = System.Math.PI
/// The constant e = 2.718281828
    let E = System.Math.E
    let Gamma_n = 10
    let Gamma_r = 10.900511
    let Gamma_dk = [| 2.48574089138753565546e-5;
                      1.05142378581721974210;
                      -3.45687097222016235469;
                      4.51227709466894823700;
                      -2.98285225323576655721;
                      1.05639711577126713077;
                      -1.95428773191645869583e-1;
                      1.70970543404441224307e-2;
                      -5.71926117404305781283e-4;
                      4.63399473359905636708e-6;
                      -2.71994908488607703910e-9 |]

    /// Computes the logarithm of the gamma function for real numbers.
    let GammaLn z =    
        /// This method computes the logarithm of the gamma function in the upper half plane.
        let gammaln_p z =
            let mutable s = Gamma_dk.[0]
            for i=1 to Gamma_n do
                s <- s + Gamma_dk.[i] / (z + (float (i-1)))
            (log s) + (log 2.0) + 0.5 * (log (E/PI)) + (z - 0.5) * (log ((z - 0.5 + Gamma_r)/E))

        if z < 0.5 then                                     // TODO how do we choose when to mirror?
            log PI - log (sin (PI*z)) - gammaln_p(1.0-z)
        else
            gammaln_p z

/// Computes the natural logarithm of the factorial function.
    let FactorialLn (x: int) : float =
        if x < 0 then failwith "Log factorial not defined for n < 0"
        if x <= 1 then 0.0 else GammaLn ((float x) + 1.0)

/// Computes the binomial coefficient.
    let myBinomial (n: int) (h: int) = floor (0.5 + exp ((FactorialLn n) - (FactorialLn h) - (FactorialLn (n-h))))

    let internalBinomial =  memoize (fun (n,h) -> myBinomial n h ) "internalBinomial"//dnAnalytics.Math.SpecialFunctions.Binomial(n,h))

    let Binomial (n: int) (h: int) = 
        match (n,h) with
            | _,0 -> 1.0
            | a,1 -> float a
            | a,b -> if a = b then 1.0 else internalBinomial.[(a,b)]//myBinomial n h

end

module StaticHelper =
    begin
        let compute_combinations(required_occurrence_number_vector:Int64 array ref) (i:Int32) (occorrences : Int64) = 
                if ((occorrences) = (!required_occurrence_number_vector:Int64 array).[i])  
                then 1L
                else (*BigInt.to_Int64*) (System.Convert.ToInt64 (MyMath.Binomial(*ChachedBinomial.get_binomial*)(System.Convert.ToInt32 occorrences)  (System.Convert.ToInt32 ((!required_occurrence_number_vector: Int64 array).[i]))))
    end

module ChachedBinomial =
    begin
        (*
        /// naive factorial is capable to compute using only integer
        /// for input value < 13 (otherwise get an overflow)
        let rec naive_factorial(n) = (
                    match n with 
                        | 0 | 1 -> 1
                        //| k  where (k > 12L) -> failwith "accept only input < 13"
                        | _ -> (n * naive_factorial(n-1))
                )
        /// iterative version of naive factorial
        let rec naive_factorial_iterative(n) = (
                    match n with 
                        | 0 | 1 -> 1
                        | _ ->  (
                                let out = ref 1;
                                for i = 1 to n do out := !out * i done
                                !out
                                )
                )
        /// compute (n * n-1 * n-2 * ... * n-k+1)
        let mul_from_to(n,k) = (
            let n_big, k_big = BigInt.of_Int64 n, BigInt.of_Int64 k
            let rec loop acc output =
                if acc >= k_big then (!output) 
                    else  loop (acc + 1I) (ref(BigInt.mul !output ((n_big - acc))))
            loop 1I (ref(n_big))
        )
        // caches
        let factorial_chache = Dictionary<_,_>()
        /// compute factorial using memoization cache
        let get_factorial(input: Int64) : BigInt  =
            let ok,res = factorial_chache.TryGetValue(input)
            if(ok) then (res)
            else (
                /// if input is suitable for naive factorial use that for better performance
                /// else use the BigInt Factorial library function
                if (input<13L)     
                    then    let res = BigInt.of_int((naive_factorial_iterative(Int32 input)))
                            factorial_chache.Add(input, res)
                            res                  
                    else    let res = BigInt.factorial(BigInt.of_Int64 (input))
                            factorial_chache.Add(input, res)
                            res                 
            )
        /// compute (n * n-1 * .. * n-k+1) using memoization cache
        let get_ntok(input_n: Int64, input_k: Int64) : BigInt  =
            mul_from_to(input_n,input_k)
        *)
      
        let get_binomial_internal(n:Int64,k:Int64) : Int64 = 
             let N = System.Convert.ToInt32 n
             let K = System.Convert.ToInt32 k
             System.Convert.ToInt64 (MyMath.Binomial N K)
        
(*
        ///  compute (n * n-1 * ... * n-k+1) / (k!)
        let get_binomial(n:Int64,k:Int64) : BigInt = (
                if (k = 1L) then BigInt.of_Int64 (n) 
                            else ( ( BigInt.div (get_ntok(n,k)) (get_factorial(k))) )
                            
         )*)
            
    end

/// a generic SymbolTable
/// that map element of type 'a (identifier) in element of type int (addresses)
//type SymbolTable  =  
//    class
//     // FIELDS
//        val mutable table :  Dictionary< int, string> 
//        val mutable inverse_table : Dictionary< string, int >
//    // CONSTRUCTORS
//        new() = { table = new Dictionary<int, string>(); inverse_table =  new Dictionary<string, int>(); }
//    // METHODS
//        /// get the number of different values
//        member inline st.Count() : int = st.table.Count
//        /// get a value by an address
//        member st.get(i: int) : string Option =
//            let ok, res = st.table.TryGetValue(i)
//            if ok then Some(res)
//                  else None
//        /// get an address by an identifier
//        member st.inverse_get(i: string) : int Option =
//            let ok, res = st.inverse_table.TryGetValue(i)
//            if ok then Some(res - 1)
//                  else None
//        /// add a value and get it's address
//        member st.add(elem : string) : int =
//            let ok,res = st.inverse_table.TryGetValue(elem)
//            if ok 
//                then (  res - 1 )
//                else (
//                    st.table.Add(st.Count(), elem )
//                    st.inverse_table.Add(elem, st.Count())
//                    st.Count()-1
//                )
//        /// remove a value
//        member st.remove(elem : string) : unit =
//            let ok,res = st.inverse_table.TryGetValue(elem)
//            if (ok) 
//                then (
//                    st.inverse_table.Remove(elem) |> ignore
//                    st.table.Remove(res) |> ignore
//                )
//                else ()
//        /// get string representation of the symbol table
//        override st.ToString() : string =
//            let mutable output = new StringBuilder("[ ")
//            for keyVal in st.table do 
//                output.Append("( " + keyVal.Key.ToString() + " , " + box(keyVal.Value).ToString() + " )") |> ignore 
//            output.Append(" ]") |> ignore
//            output.ToString()
//    end
/// represent an address in a SymbolTable
type addr = Int32    
/// represent a generic SCLS term (abstract class)
[<AbstractClass>]
type Node =
    class
     // FIELDS
     // CONSTRUCTORS
        new() = {parent = None}
     // METHODS
        /// return the size (=number of element) in the node
        abstract member Size : Int64 
        /// get string representation
        abstract member ToString : SymbolTable -> string
        /// the parent of the node
        val mutable parent : Node Option
     // STATIC MEMBERS
        override x.GetHashCode() = LanguagePrimitives.GenericHash(x)
        override x.Equals(y:obj) = LanguagePrimitives.GenericHash(x) = LanguagePrimitives.GenericHash(y)
        /// get a Node (with variables) and a bindings (a map between variable identifier and node)
        /// and return a new node where the variables are replaced by the bindings associated node
        /// note : it is not guaranteed to obtains a ground term; the ratio is to concur partial instantiations
        /// to force to obtain a ground term select between commented line witch is more adapt to required behaviour
        /// assumptions : in a bindings worth the following rules
        ///     sequence variable   -   not bound                  = None
        ///                         -   bound to empty sequence    = Some(new Sequence([]))
        ///                         -   bound to sequence          = Some(_) 
        ///     element variable     -   not bound                  = None
        ///                         -   bound to element           = Some(new Sequence([Element(_)]))  
        ///     sequence variable   -   not bound                  = None
        ///                         -   bound to empty term        = Some(new Compartment(empty_dictionary))
        ///                         -   bound to term              = Some(new Compartment(_))          
        static member instantiateNode(n:Node, b:bindings (*, parent:Node Option*)) : Node =  (
                    match n with
                        /// case of Sequence or LoopingSequence
                        |   :? LoopingSequence as seq ->   
                                (
                                let newChildrenList = new ResizeArray<Element>()
                                /// replace an element of the sequence with bound one
                                let replace = 
                                    fun (e:Element) ->
                                        let inst_el = 
                                            match e with
                                                |   :? ElementVariable as ev -> 
                                                        match b.[ev.address] with
                                                            | Some(a) -> 
                                                                match a with 
                                                                    |   :? Sequence as s -> [s.children.[0]] 
                                                                    |   _ -> failwith "wrong binding" 
                                                            |  None -> [e]
                                                          //|  None -> []                               - if want anyway a ground term (not correct for CLS)
                                                          //|  None -> failwith "not closing binding"   - if want a ground term
                                                |   :? SequenceVariable as sv -> 
                                                        match b.[sv.address] with
                                                            | Some(a) -> 
                                                                match a with 
                                                                    |   :? Sequence as s -> List.ofSeq s.children  
                                                                    |   _ -> failwith "wrong binding"
                                                            |  None -> [e]  // or -> [] or -> failwith
                                                |   _ -> [e]                // or -> [] or -> failwith
                                        newChildrenList.AddRange(inst_el)

                                /// replace each element in the sequence with the bound one
                                Seq.iter replace seq.children
                                /// build a new sequence with the list of instantiated elements
                                (new LoopingSequence(newChildrenList,seq.parent) :> Node )
                                )
                        |   :? Sequence as seq ->   
                                (
                                let newChildrenList = new ResizeArray<Element>()
                                /// replace an element of the sequence with bound one
                                let replace = 
                                    fun (e:Element) ->
                                        let inst_el = 
                                            match e with
                                                |   :? ElementVariable as ev -> 
                                                        match b.[ev.address] with
                                                            | Some(a) -> 
                                                                match a with 
                                                                    |   :? Sequence as s -> [s.children.[0]] 
                                                                    |   _ -> failwith "wrong binding" 
                                                            |  None -> [e]
                                                          //|  None -> []                               - if want anyway a ground term (not correct for CLS)
                                                          //|  None -> failwith "not closing binding"   - if want a ground term
                                                |   :? SequenceVariable as sv -> 
                                                        match b.[sv.address] with
                                                            | Some(a) -> 
                                                                match a with 
                                                                    |   :? Sequence as s -> List.ofSeq s.children  
                                                                    |   _ -> failwith "wrong binding"
                                                            |  None -> [e]  // or -> [] or -> failwith
                                                |   _ -> [e]                // or -> [] or -> failwith
                                        newChildrenList.AddRange(inst_el)

                                /// replace each element in the sequence with the bound one
                                Seq.iter replace seq.children
                                /// build a new sequence with the list of instantiated elements
                                (new Sequence(newChildrenList,seq.parent) :> Node )
                                )
                        |   :? Loop as loop -> 
                                (
                                /// build a new Loop with instantiated membrane and content
                                let instantiated_membrane   = (* let seq = *)(Node.instantiateNode((loop.membrane :> Node), b(*, Some(loop :> Node)*)) :?> LoopingSequence) (*in new LoopingSequence(seq.children, Some(loop :> Node))*)
                                let instantiated_content    =  Node.instantiateNode((loop.content :> Node), b(*, Some(loop :> Node)*)) :?> Compartment
                                instantiated_membrane.putInNormalForm()
                                new Loop(   (instantiated_membrane) ,   (instantiated_content), loop.parent )    :> Node
                                )                
                        |   :? Compartment as comp_in ->
                                (
                                let comp = new Compartment(None)
                                for keyVal in comp_in.children do 
                                   match keyVal.Key with
                                        | :? TermVariable as tv -> 
                                                match b.[tv.address] with
                                                        | Some(newv) -> comp.AddChild(newv, keyVal.Value,true) 
                                                        | None -> ()   
                                        | _ ->  let inst = Node.instantiateNode(keyVal.Key,b)
                                                comp.AddChild(inst, keyVal.Value,true)
                                done
                                comp :> Node
                                )    
                        |   _ -> failwith " Runtime error in binding "
            )
        abstract member Clone : unit -> Node
        interface System.IComparable with
            member x.CompareTo(y:obj) = 
                match y with
                    |   :? Node as n    -> if x = n then 0 else 1
                    |   _                   -> -1
        end 
    end
/// represent a generic element (abstract class) 

//and //[<AbstractClass>]
and Element = 
    class
     // FIELDS
        /// the address of the element in the symbol table of the SCLS model
        val address : addr
     // CONTRUCTOR
        new(a) = { address = a }
     // METHODS
        /// get string representation
        override e.ToString() : string = e.address.ToString()   
        /// get string representation with address translated to identifier
        member e.ToString(s: SymbolTable) : string = let ide = s.get(e.address) in if ide.IsSome then ide.Value else ""     
        /// object equality
        override x.Equals(y:obj) =
            match y with 
                |   :? Element as el    -> x.address = el.address 
                |   _                   -> false 
        //override x.GetHashCode() = LanguagePrimitives.GenericHash(x)
     // INTERFACES
        /// get structural hash code (called by LanguagePrimitives.GenericHash(_) )
        override x.GetHashCode() = 
            x.address
        //interface IStructuralHash with
        //   member x.GetStructuralHashCode(_) =  x.address 
        //end 
        /// object comparator
        interface System.IComparable with
            member x.CompareTo(y:obj) = 
                match y with
                    |   :? Element as el    ->  x.address.CompareTo(el.address)
                    |   _                   -> -1
        end
        member x.Clone() = new Element(x.address)
    end
/// represent a constant element
and ConstantElement = 
    class
        inherit Element //as base  
        /// constructor
        new(a) = {inherit Element(a) }
        member x.Clone() = new ConstantElement(x.address)//x:> Element//new ConstantElement(x.address)  :> Element //UNCLONE
    end
/// represent a variable of type element    
and ElementVariable = 
    class
        inherit Element //as base
        /// constructor
        new(a) = {inherit Element(a) }
        /// get string representation
        override e.ToString() : string = "$e:" + (e:>Element).address.ToString()
        /// get string representation with address translated to identifier
        member e.ToString(s: SymbolTable) : string = let ide = s.get((e:>Element).address) in if ide.IsSome then "$e:" + ide.Value else ""
        member x.Clone() = new ElementVariable(x.address)//x :> Element//new ElementVariable(x.address)  :> Element //UNCLONE
    end
/// represent a variable of type sequence
and SequenceVariable = 
    class
        inherit Element //as base
        /// constructor
        new(a) = {inherit Element(a) }
        /// get string representation
        override e.ToString() : string = "$s:" + (e:>Element).address.ToString()
        /// get string representation with address translated to identifier
        member e.ToString(s: SymbolTable) : string = let ide = s.get((e:>Element).address) in if ide.IsSome then "$s:" + ide.Value else "" 
        member x.Clone() = new SequenceVariable(x.address)// x :> Element// new SequenceVariable(x.address)  :> Element // UNCLONE
    end
/// represent a variable of type term
and TermVariable = 
    class
        inherit Node //as base
     // FIELDS 
        val address : addr
     // CONSTRUCTORS
        new(a) as x = { address = a } then (x:>Node).parent <- None
        new(a, p) as x = { address = a } then (x:>Node).parent <- p
     // METHODS
        /// get string representation   
        override e.ToString() : string = "$t:" + e.address.ToString()
        /// get string representation with address translated to identifier
        override e.ToString(s: SymbolTable) : string = let ide = s.get(e.address) in if ide.IsSome then "$t:" + ide.Value else "" 
        /// get size
        override x.Size = 1L
        /// object equality
        override x.Equals(y:obj) =
            match y with 
                |   :? TermVariable as tv   -> x.address = tv.address 
                |   _                       -> false 
     // INTERFACES
        /// get structural hash code (called by LanguagePrimitives.GenericHash(_) )
        override x.GetHashCode() = x.address 
        //interface IStructuralHash with
        //   member x.GetStructuralHashCode(_) =  x.address 
        //end 
        /// object comparator
        interface System.IComparable with
            member x.CompareTo(y:obj) = 
                match y with
                    |   :? TermVariable as tv   -> x.address.CompareTo(tv.address)
                    |   _                       -> -1
        end
        override x.Clone() = x :> Node//new TermVariable(x.address)  :> Node //UNCLONE
    end
/// represent term made by "parallel composition" operator
and Compartment =
    class
        inherit Node //as base
     // FIELDS
        /// hash table that contains elements and their occurrences
        val children :  Dictionary< Node, Int64 > 
     // CONSTRUCTORS  
        /// build an empty compartment

        new(?p) as x  =  { children = new Dictionary<_,_>(HashIdentity.Structural) } then 
                       (x:>Node).parent <- match p with None -> None | Some(pp) -> pp
        /// build a compartment with specified children
        /// note : there is the assumption that the dictionary already do not contains duplicates
        new(c:Dictionary<Node, Int64>, ?p) as this  =   
            {children = new Dictionary<_,_>(HashIdentity.Structural); }
            then
            this.parent <- match p with None -> None | Some(pp) -> pp
            let parent_of_parent = 
                match this.parent with
                    | None -> None
                    | Some(p) ->  p.parent
            this.removeUp(parent_of_parent)
            for keyVal in c do 
                this.AddChild(keyVal.Key, keyVal.Value, false)
            done
            this.addUp(parent_of_parent)
     // METHODS
        /// remove from term the path from this node to the root of term
        member c.removeUp(parent_of_parent : Node Option) =
            if parent_of_parent.IsSome 
                then    
                 match parent_of_parent.Value with
                            | :? Compartment as cc ->   cc.RemoveChild(c.parent.Value, 1L) 
                            | _ -> (failwith " Runtime error in hash chain ")
       
        /// add to the term the path from this node to the root of term
        member c.addUp(parent_of_parent: Node Option) =
            if parent_of_parent.IsSome 
                    then    
                        match parent_of_parent.Value with
                            | :? Compartment as cc ->   cc.AddChild(c.parent.Value, 1L,true)
                            | _ -> (failwith"!!")
  
        /// add a occurrence number of child to children 
        member c.AddChild((node, occ, update):(Node*Int64*bool) ) : unit =
            let update_hash_chain = update 
            let parent_of_parent = 
                if update_hash_chain 
                    then
                        match c.parent with
                            | None -> None
                            | Some(p) ->  p.parent
                    else None
            if update_hash_chain then c.removeUp(parent_of_parent)
            match node with 
                /// if try to add a compartment then add its children directly
                | :? Compartment as comp -> 
                    for subChild in comp.children do 
                        c.AddChild(subChild.Key, subChild.Value * occ , false) 
                    done 
                | _ -> 
                    node.parent <- Some(c :> Node)
                    /// check if the compartment already have that child
                    let ok,value = c.children.TryGetValue(node)
                    //match c.children.TryFind(node)  with
                    if ok 
                        then  ( c.children.[node]<- (value + occ)   )
                        /// else add as new 
                        else  (
                            // if not ((node :? Sequence) && ((node :?> Sequence).children.Length = 0 )) then
                            // if uncommented do not permit to add an empty sequence 
                            c.children.Add(node,occ)
                            )
            //node.parent <- Some(c :> Node)
            if update_hash_chain then c.addUp(parent_of_parent)
            
        /// remove a child from children                              
        member c.RemoveChild(child: Node, times:Int64, ?update:bool) =
            let update_hash_chain = match update with None -> true | Some(b) -> b
            let parent_of_parent = 
                match c.parent with
                    | None -> None
                    | Some(p) ->  p.parent
            if update_hash_chain then c.removeUp(parent_of_parent)
            /// check if the compartment contains that child
            let ok,value = c.children.TryGetValue(child) 
            if ok
                then
                    /// if so check the number of occurrence of that child    
                    /// if try to remove equals or more than how many child are present; then remove the child
                    if(value <= times)      then    (c.children.Remove(child)|> ignore)
                    /// else only decrement it occurrence counter
                                            else    (c.children.[child] <- value- times)
                ///else do nothing (there are nothing to remove) 
                else 
                    ( /// debug prints : we should never get here
                    Console.WriteLine("CAN'T FIND " + child.ToString()  + " IN " + c.ToString() +  " " + child.GetType().ToString() +  " " + child.GetHashCode().ToString()+  " " + LanguagePrimitives.GenericHash(child).ToString()) //DEBUG
                    Console.WriteLine("Keys :")
                    for keyVal in c.children do
                        Console.WriteLine("\t" + keyVal.Key.ToString() +  " " + keyVal.Key.GetType().ToString() + " " + keyVal.Key.GetHashCode().ToString()+  " " + LanguagePrimitives.GenericHash(keyVal.Key).ToString())
                    done
                    Console.WriteLine("To Remove :")
                    Console.WriteLine("\t" +  child.ToString() + " " + child.GetHashCode().ToString()+  " " + LanguagePrimitives.GenericHash(child).ToString())
                    )
            if update_hash_chain then c.addUp(parent_of_parent)
        
        /// remove the children of what from who and add the child of withwhat
        /// (here can do better by removing only the difference [what-withwhat] and adding [withwhat-what])
        member c.Replace((who: Node ), (whatSearched:Node) , (withWhat: Node) ) : unit = 
            match whatSearched with
                |   :? Compartment as comp ->
                        if LanguagePrimitives.PhysicalEquality who (c :> Node) 
                            then (
                                    let parent_of_parent = 
                                        match c.parent with
                                            | None -> None
                                            | Some(p) ->  p.parent
                                    for keyVal in comp.children do
                                         c.RemoveChild(keyVal.Key,keyVal.Value,false) //era true
                                         //c.RemoveChild(keyVal.Key,keyVal.Value,true) //giusto sicuro
                                    done
                                    //c.AddChild(withWhat,1L,false)
                                    c.AddChild(withWhat.Clone(),1L,false) //era true UNCLONE
                                    //c.AddChild(withWhat.Clone(),1L,true) //giusto sicuro
                            )
                        else (
                            /// call recursively on the content of loop children
                            for keyVal in (*List*)Array.ofSeq c.children do
                                    match keyVal.Key with
                                        | :? Loop as loop ->    
                                            let parent_of_parent = 
                                                match c.parent with
                                                    | None -> None
                                                    | Some(p) ->  p.parent
                                            c.RemoveChild(loop,keyVal.Value,false);
                                            //c.RemoveChild(loop,keyVal.Value,true);//giusto sicuro
                                            let oldloop = loop.Clone() //UNCLONE
                                            loop.content.Replace(who,whatSearched, withWhat);
                                            c.AddChild( (loop:>Node) (*.Clone()*), 1L(*keyVal.Value*),false)  //UNCLONE
                                            if keyVal.Value > 1L then c.AddChild(oldloop(*.Clone()*),keyVal.Value - 1L,false) //UNCLONE
                                            //c.AddChild(loop.Clone(), 1L(*keyVal.Value*),true) //giusto sicuro
                                            //if keyVal.Value > 1L then c.AddChild(oldloop.Clone(),keyVal.Value - 1L,true)//giusto sicuro
                                        | _ -> ()
                            done           
                            )
                | _ -> ()
           
        /// get the number of children (summary of the repetitions of every son)
        member c.childCount =
            Seq.fold (fun acc (keyVal:KeyValuePair<_,_>) -> acc + keyVal.Value ) 0L c.children 
             
        /// true iff the compartment has some child that are of type Loop
        member c.hasLoop : bool =
             c.children |> Seq.exists (fun keyVal -> keyVal.Key :? Loop)
            
        /// return the size (= number of elements) of the compartment  
        override c.Size : Int64 =
            let outsize = ref 0L
            for keyVal in c.children do
                match keyVal.Key with
                    | :? Compartment as child   -> ( outsize := !outsize + (child.Size * keyVal.Value ))
                    | :? Loop as loop           -> ( outsize := !outsize + (loop.Size * keyVal.Value  )) 
                    | :? Sequence as seq        -> ( outsize := !outsize + (seq.Size * keyVal.Value   )) 
                    | :? TermVariable           -> ( outsize := !outsize + (1L  * keyVal.Value        ))
                    | _ -> ()  
            done
            !outsize
            
        /// return the height (= maximum height of the tree ) of the compartment 
        member c.Height : int =
            let maxHeight = ref 0
            for keyVal in c.children do
                match keyVal.Key with
                    | :? Compartment as child -> ( if(child.Height > !maxHeight) then maxHeight := child.Height  )
                    | :? Loop as loop ->    (   let contentHeight = loop.content.Height
                                                if(contentHeight > !maxHeight) then maxHeight := contentHeight   )
                    /// in case of Sequence and Element
                    | _ -> if maxHeight = ref 0 then maxHeight := 1 
            done
            ///     add the height of compartment (=1) and then return
            1 + !maxHeight
     
        /// get string representation  
        override c.ToString() : string =   
            //let parent_str = match c.parent with None -> "_" | Some(p) -> p.ToString()
            if c.children.Count > 0 
                then  
                    let output = new StringBuilder()
                    for keyVal in c.children 
                        do (
                            let number = keyVal.Value 
                            let node = keyVal.Key
                            if(number <> 1L) then output.Append((node.ToString() + "*" + number.ToString() + "|"):string) |> ignore
                            else output.Append((node.ToString() + "|"):string) |> ignore  
                    )
                    (if output.Length <> 0 then output.Remove(output.Length - 1, 1 )|> ignore)            
                    output.ToString()// + " with parent " + parent_str
                else    
                    "_" //+ " with parent " + parent_str  /// the empty compartment has "_" as string representation
        
        /// get string representation with address translated to identifier
        override c.ToString(s:SymbolTable) : string =   
            //let parent_str = match c.parent with None -> "_" | Some(p) -> p.ToString()
            if c.children.Count > 0 
                then  
                    let output = new StringBuilder()
                    for keyVal in c.children 
                        do (
                            let number = keyVal.Value 
                            let node = keyVal.Key
                            if(number <> 1L) then output.Append((node.ToString(s) + "*" + number.ToString() + " | "):string) |> ignore
                            else output.Append((node.ToString(s) + " | "):string) |> ignore  
                    )
                    (if output.Length <> 0 then output.Remove(output.Length - 2, 2 )|> ignore)            
                    output.ToString()// + " with parent " + parent_str 
                else    
                    "_" //+ " with parent " + parent_str  /// the empty compartment has "_" as string representation
                    
        /// find all occurrences of a what Node and replace all with withWhat Node 
        member c.ReplaceAll((what: Compartment ), (withWhat: Node) ) : unit = 
            let listOfOccourrences = c.find(what)
            for (occ, numberOf) in listOfOccourrences do
                c.Replace(occ,what, withWhat)
            done
           
        /// find a Node in this compartment
        /// return a list of pair (ref to the occurrences, number of occurrences in that Node)  
        member c.find(what : Compartment) : (Node * Int64) ResizeArray = 
            let output = new ResizeArray<_>()
            let mutable occurrences = 0L
            /// if what I search is exactly the same of current node signal an occurrence and return
            /// it is not possible to find it again between children 
            if what = c then 
                (output.Add(((c :> Node), 1L)))
            else (
            /// else search as sub compartment of this node and in this node children
            /// local search 
                /// count the number of occurrences of what in this node
                occurrences <- c.contain(what) 
                /// if what are included in this node then add at the list of occurrences
                /// a ref to this node with the number of occurrences of what in this
                if occurrences > 0L then output.Add(((c :> Node), occurrences)) 
                for keyVal in c.children do 
                    match keyVal.Key with
                        | :? Compartment as comp -> output.AddRange(comp.find(what)) 
                        | :? Loop as loop -> output.AddRange(loop.content.find(what))
                        | _ -> ()    
                done
            /// recursive search in the content of the child of type Loop
                for keyVal in c.children do 
                    match keyVal.Key with
                        |   :? Loop as loop ->  
                                // search in the content
                                output.AddRange((loop.content).find(what)) 
                                /// search in the membrane 
                                /// look if what searched is 1 child compartment with the only child = membrane
                                if what.children.Count = 1 && what.childCount = 1L  then (
                                    let onlyChild = (Array.ofSeq what.children).[0]
                                    match onlyChild.Key with
                                        | :? LoopingSequence as ls -> if ls = loop.membrane then output.Add( ((loop.membrane :> Node),onlyChild.Value) )
                                        | _ -> ()                     
                                 )
                        | _-> ()
                done
            )                                         
            output
            
        ///     check if this compartment contain another compartment
        ///     according to "general notion of occurrence"
        ///     [ a compartment contains another compartment if it contains each of its child ]
        ///     return the number of occurrence of what in this compartment
        member c.contain(what: Compartment) : Int64 =
            let output = ref (Array.zeroCreate (what.children.Count) )
            /// will contains the number of each child that are required
            let required_occurrence_number_vector = ref (Array.zeroCreate (what.children.Count) )
            /// a flag that indicates if all the necessary child are founds
            let allFound = ref true
            let counter = ref 0 
           
            let allFound = 
                what.children
                |> Seq.forall
                    (    
                    fun keyVal ->
                    (
                    counter := !counter + 1
                    let (child_to_find, required_occurrence_number) = (keyVal.Key, keyVal.Value)
                    /// check if this compartment contains current what's child
                    //match c.children.TryFind(child_to_find) with
                    let ok,found_occurrence_number = c.children.TryGetValue(child_to_find)
                    if ok
                        then
                            /// if this compartment contains current what's child
                            /// check if there are sufficient occurrences 
                            if(found_occurrence_number < required_occurrence_number )
                                /// current what's child is found but are present in not sufficient number 
                                then ( false) 
                                /// current what's child is found and sufficient number
                                else (  /// store the number of child found and the number of child necessary
                                        (!required_occurrence_number_vector:Int64 array).[!counter - 1] <- required_occurrence_number
                                        (!output:Int64 array).[!counter - 1] <- found_occurrence_number
                                        true
                                    )        
                        /// current what's child is not found 
                        else false 
                        )
                    )
            /// (here output contains the number of occurrence of each found child)
            /// test if had find all what's child in sufficient number
            if (not allFound) 
                then    ( 0L )     /// there are some child that are not present or that are present in not sufficient number
                else    (          /// all what's child are found in sufficient number
                        /// transform the list of numbers of occurrence of each found child
                        /// in the list of number of ways in which the children can be taken
                        Array.fold (fun (accumulator) (x) -> (accumulator * (x)) ) (1L) (Array.mapi (StaticHelper.compute_combinations(required_occurrence_number_vector)) (!output))
                        )

        /// get the list of Node that are leaves of this compartment
        member c.getConcentrations(numberOfElements : int) : Int64 array =
            let mergeConcentrations((a: Int64 array), (b: Int64 array), (rep:Int64))  =
                    Array.iteri (fun i el ->  a.[i] <- el + (b.[i] * rep ) ) a
            let output = Array.zeroCreate numberOfElements 
            for keyVal in (*Array.ofSeq*) c.children  do
                match keyVal.Key with
                    |   :? Loop as loop         -> 
                            Seq.iter (fun (el:Element) ->  output.[el.address] <- output.[el.address] + keyVal.Value) loop.membrane.children
                            mergeConcentrations(output, loop.content.getConcentrations(numberOfElements), keyVal.Value)
                    |   :? Sequence as seq      ->       
                            Seq.iter (fun (el:Element) ->  output.[el.address] <- output.[el.address] + keyVal.Value) seq.children
                    |   :? Compartment  as comp ->  mergeConcentrations(output, comp.getConcentrations(numberOfElements), keyVal.Value)
                    |   _ -> ()
            done
            output
            
        //static member difference(c1:Compartment, c2:Compartment) : Compartment =  // TODO: restituisce la differenza tra il primo ed il secondo
        //static member shared(c1:Compartment, c2:Compartment) : Compartment =      // TODO: restituisce la parte che hanno in comune il primo ed il secondo
        
        /// count the sum of occurrences of each element (by its identifier) in the compartment 
        member s.count(what: int array) : Int64 =
            let acc = ref 0L
            for keyVal in s.children do
                match keyVal.Key with
                    |   :? Sequence as s -> acc:= !acc + (s.count(what) * keyVal.Value)  
                    |   :? Loop as l -> acc := !acc + ((l.membrane.count(what) + l.content.count(what)) * keyVal.Value)
                    | _ -> () 
            done
            !acc
        
        /// get the a triple of lists of address of the variables
        /// (there are like multiset; to get the set of different variable it is necessary to call Set.ofList)
        member s.getVariables() : (*ElementVariable*)ResizeArray<addr> * (*SequenceVariable*) ResizeArray<addr> * (*TermVariable*) ResizeArray<addr> =
            let el_output = new ResizeArray<addr>()
            let se_output = new ResizeArray<addr>()
            let te_output = new ResizeArray<addr>()
           
            let getVars(input:ResizeArray<Element>) = 
                let e_out = new ResizeArray<addr>()
                let s_out =  new ResizeArray<addr>()
                for i in input do
                    match i with
                        |   :?  ElementVariable as el -> e_out.Add(el.address)
                        |   :?  SequenceVariable as se ->  s_out.Add(se.address) 
                        |   _ -> ()                        
                done
                e_out, s_out
            
            for keyVal in s.children do
                match keyVal.Key with
                    |   :? Sequence as seq -> 
                                            let e,s = getVars(seq.children)
                                            el_output.AddRange(e)
                                            se_output.AddRange(s)
                    |   :? Loop as loop ->  
                                            let e1,s1 =  getVars(loop.membrane.children)
                                            let e,s,t = loop.content.getVariables()
                                            el_output.AddRange(e1);el_output.AddRange(e);
                                            se_output.AddRange(s1);se_output.AddRange(s);
                                            te_output.AddRange(t)
                                            
                    // NEVER REACHED
                    //|   :? LoopingSequence as ls -> 
                    //                                let e,s = getVars(ls.children)
                    //                                el_output.AddRange(e)
                    //                                se_output.AddRange(s)
                                                   
                    |   :? TermVariable as tv -> te_output.Add(tv.address) 
                    |   _ -> ()
            el_output,se_output,te_output
        
        /// compute the power set of the children
        static member power_set(input: Compartment) : Compartment ResizeArray =
            let rec p=function a::b->p b@List.map((@)[a])(p b)|l->[l]
            let flatten =
                let out = ref []
                for keyVal in input.children do
                    for i= 0 to int keyVal.Value do
                        out := keyVal.Key :: !out 
                    done
                done
                !out
            let output = new ResizeArray<_>(System.Convert.ToInt32 (System.Math.Pow(2.0, float input.childCount ))) 
            for list in p flatten do
                let newc = new Compartment()
                for c in list do 
                    newc.AddChild(c,1L,true);
                done
                output.Add(newc);
            done
            output
        
        /// object equality 
        override x.Equals(y:obj) =
            match y with 
                |   :? Compartment as comp -> 
                        (x.children.Count =  comp.children.Count) &&
                        (
                        comp.children
                        |> Seq.forall
                            (
                            fun keyVal ->
                            let ok,value = x.children.TryGetValue(keyVal.Key)
                            ok && value = keyVal.Value
                            )
                        )
                | _ -> false
                
       // INTERFACES
        /// get structural hash code (called by LanguagePrimitives.GenericHash(_) )
        //interface IStructuralHash with
        //    member x.GetStructuralHashCode(_) =  
        override x.GetHashCode() =
                x.children
                |> Seq.fold
                    (
                    fun output child ->
                    //output +++ (( (*11 **) child.Key.GetHashCode()) +++ ((*5 **) int child.Value)) (*(Int32 (child.Value % Int64 Int32.max_int))))*)
                    output ^^^ (( 11 * child.Key.GetHashCode()) ^^^ (5 * int child.Value)) (*(Int32 (child.Value % Int64 Int32.max_int))))*)
                    ) 1
        //end 
        
        /// object comparator
        interface System.IComparable with
            member x.CompareTo(y:obj) = 
                match y with 
                    |   :? Compartment as comp -> if x.Equals(comp) then 0 else 1
                    |   _ -> -1
        end
        /// clone the compartment recursively (namely clone each node till the leaves)
        override x.Clone() = 
            let newv = new Compartment(x.parent);
            for keyVal in x.children do
                newv.children.Add(keyVal.Key.Clone(), keyVal.Value)
            done  
            newv :> Node
    end
    
/// represent term made by "sequence of elements" operator
and Sequence =
    class
        inherit Node //as base
      /// FIELDS
        /// an ordered list of sequence element 
        val mutable children : ResizeArray<Element>     
            // NOTE: here we can do it better: we can develop a compact representation of sequences
            // ex: instead of a.a.a.a.a.b.c.c.d we should use a*5.b.c*2.d
      /// CONSTRUCTORS
        /// make a sequence with empty list of element or with specified list of elements 
        new(s:string,p) as this =
            { children = new ResizeArray<Element>()}
            then (this:>Node).parent <- p
        new(s:ResizeArray<Element>,p ) as this = 
            { children = s } 
            then (this:>Node).parent <- p
        new(s:ResizeArray<Element> ) as this = 
            { children = s} 
            then (this:>Node).parent <-  None
        new(s:Element array,?p ) as this = 
            { children =   let cc = new ResizeArray<Element>() in cc.AddRange(Seq.ofArray s); cc } 
            then (this:>Node).parent <- match p with None -> None | Some(pp) -> pp
      /// METHODS          
        /// add an element in tail of sequence
        member s.AddChild(elem : Element) : unit        = s.children.Add(elem)
        /// add an element in head of sequence      
        //member s.AddChildBack(elem : Element) : unit    = s.children <- elem :: s.children
        /// return the size (= number of elements) of the sequence  
        override s.Size : Int64 = System.Convert.ToInt64 (s.children.Count)
        /// return the height (= height of the tree = 1 in the case of sequences) of the sequence      
        member s.Height : int = 1  
        /// the sequence is ground if contains only ConstantElement      
        member s.isGround() : bool =
            let ground = ref true
            if (!ground) then
                for e in s.children do
                    match e with
                        |   :? ConstantElement -> ()
                        |   :? ElementVariable -> ground := false;
                        |   :? SequenceVariable -> ground := false;
                        | _ -> failwith "state corruption : an abstract element inside a sequence"
                done
            !ground
            
        /// get string representation
        override s.ToString() : string =
            ///let parent_str = match s.parent with None -> "_" | Some(p) -> p.ToString()
            match s.children.Count with
                |   0 -> "_" 
                |   1 -> (s.children.[0]).ToString()
                |   _ -> (
                            let output = new StringBuilder()
                            for node in s.children do
                                output.Append(node.ToString() + ".") |> ignore
                            done
                            output.Remove(output.Length - 1, 1 ) |> ignore
                            output.ToString()
                          )
            //+ " with parent " + parent_str
            
        /// get string representation with address translated to identifier
        override s.ToString(ss: SymbolTable) : string =
            ///let parent_str = match s.parent with None -> "_" | Some(p) -> p.ToString()
            match s.children.Count with
                |   0 -> "_" 
                |   1 -> (s.children.[0]).ToString(ss)
                |   _ -> (
                            let output = new StringBuilder()
                            for node in s.children do
                                output.Append(node.ToString(ss) + ".") |> ignore
                            done
                            output.Remove(output.Length - 1, 1 ) |> ignore
                            output.ToString()
                          )
            //+ " with parent " + parent_str
        
        /// count the occurrences of each element in what
        member s.count(what: int array) : Int64 =
            let acc = ref 0L
            for c in s.children do
                if Array.exists (fun i -> i = c.address) what 
                    then acc := !acc + 1L 
            done
            !acc
        
        /// object equality
        override x.Equals(y:obj) =
            match y with
                |   :?  Sequence as s   ->  (x.children.Count = s.children.Count ) 
                                                && (
                                                    let i= ref (-1)
                                                    Seq.forall (fun (e1:Element) ->i:=!i+1; e1.Equals(s.children.[!i]) )  (x.children) 
                                                   ) 
                |   _                   ->  false
                
       // INTERFACES
        /// get structural hash code (called by LanguagePrimitives.GenericHash(_) )
        //interface IStructuralHash with
         //   member x.GetStructuralHashCode(_) =  
        override x.GetHashCode() = Seq.fold (fun output (child:Element) -> (output * (*5*)70(*dimensione alfabeto*) +  child.address) (* % Int32.max_int*)) 1 x.children
        //end 
        
        /// object comparator
        interface System.IComparable with
            member x.CompareTo(y:obj) = 
                match y with 
                    |   :?  Sequence as s -> if x.Equals(s) then 0 else 1
                    |   _   -> -1
        end
        
        /// clone each element in the sequence and return a cloned sequence
        override x.Clone() = 
            let newv = new Sequence(new ResizeArray<_>(), x.parent);
            for c in x.children do
                newv.children.Add(c.Clone())
            done  
            newv :> Node
     end   
      
/// represent term of type "sequence of elements" that is also a membrane
/// therefore it can rotate and it is stored in its normal form (= the minimum rotation in lexicographical order)
and LoopingSequence =
    class
     // FIELDS
        /// inherit the fields of Sequence
        inherit Sequence //as base
     // CONSTRUCTORS
        /// make a membrane with specified list of elements
        /// (make a sequence and then put it in normal form = the minimum of all the rotations )
        new(?s, ?p) as this = 
            match s , p with
                | None , None -> {inherit Sequence([||])}
                | Some(ss), None -> {inherit Sequence(ss)}  
                | None, Some(parent) -> {inherit Sequence([||],parent)}
                | Some(ss), Some(parent) -> {inherit Sequence(ss,parent)}
               then this.putInNormalForm(); 
     // METHODS
        /// put the sequence in normal form (= the minimum of all the rotations in lexicographical order)   
        member s.putInNormalForm() : unit =
            if s.children.Count > 1 then
                /// assign the min (the normal form) to this sequence
         (*       let a = s.normalForm()
                for (e:Element) in a do
                    Console.WriteLine(e.ToString())
                done
                Console.WriteLine("")
                let a = s.normalForm2()
                for (e:Element) in a do
                    Console.WriteLine(e.ToString())
                done
 
                Console.WriteLine((s.normalForm() = s.normalForm2()).ToString())
           *)   s.children <- let c = new ResizeArray<Element>() in c.AddRange(Seq.ofArray (s.normalForm())); c
                //Console.WriteLine(s.ToString())
        
        ///     make the list of all possible rotation of this membrane   // NOTE: here we can do it better
        member s.allRotations() : list<Element> array =
            if s.children.Count = 1 
                then
                    [|List.ofSeq s.children|]
                else
                    let out = Array.zeroCreate s.children.Count
                    let temp = ref ( List.ofSeq s.children)
                    let rotate(k:int) =
                        for i=1 to k do
                            temp := !temp @ [(List.head !temp)]
                            temp := List.tail !temp
                        done
                    Seq.iteri 
                        (
                            fun i e ->
                            rotate(1)
                            out.[i] <- !temp 
                        )
                        s.children
                    out
            
        member s.normalForm() : Element array  =
            if s.children.Count = 1 
                then
                    (Seq.toArray s.children)
                else
                    //let ar =Array.sortBy (LanguagePrimitives.GenericComparison )  (s.allRotations()) 
                    let ar = Array.sort (s.allRotations()) 
                    (Array.map (fun (l:_ list) -> Array.ofList l) ar).[0]
                  
       (*
        member s.putInNormalForm2() : unit =
            if s.children.Count > 1 then
                /// assign the min (the normal form) to this sequence
                s.children <- s.normalForm()//s.allRotationsSorted().[0]
                //Console.WriteLine(s.ToString())
        
        
        ///     make the list of all possible rotation of this membrane   
        member s.allRotations2() : Element array array = 
            let t = base.children.Count
            if t = 1 
                then 
                    [|  Array.of_ResizeArray base.children |]
                else
                    let local = Array.of_ResizeArray base.children
                    let temp = Array.zeroCreate(t)
                    for i=0 to t do
                        //for j= 0 to t-1 do
                            temp.[i]<-
                                Array.mapi (fun ii e -> local.[ii+i % t]) local
                                //Array.append (Array.sub local (i+1) (t-1)  ) (Array.sub local 0 i)
                        //done
                    done
                    //temp.[t-1] <- local
                    //Array.sort (LanguagePrimitives.StructuralComparison) temp
                    temp
        (*
        member s.allRotationsSorted()  =
            let ar =(s.allRotations()) 
            Array.sort (function a -> function b -> if a=b then 0 else if a<b then -1 else 1) ar
            Array.map (fun (l:_ list) -> List.to_ResizeArray l) ar*)
        
        member s.normalForm2() =
            let t = base.children.Count
            if t = 1 
                then 
                    base.children 
                else
                    let local = Array.of_ResizeArray base.children
                    let temp = Array.zeroCreate(t)
                    for i=1 to t do
                        //for j= 0 to t-1 do
                            temp.[i-1]<-
                                Array.mapi (fun ii e -> local.[(ii+i+1 % (t-1))]) local
                                //Array.append (Array.sub local (i+1) (t-1)  ) (Array.sub local 0 i)
                        //done
                    done
                    Array.sort (LanguagePrimitives.StructuralComparison) temp
                    Array.to_ResizeArray temp.[0]
           
            
                        
       *)
        /// object equality    
        override x.Equals(y:obj) = 
            match y with 
                |   :? LoopingSequence as ls    -> (x.children.Count = ls.children.Count ) 
                                                    && (
                                                        let i= ref (-1)
                                                        Seq.forall (fun (e1:Element) ->i:=!i+1; e1.Equals(ls.children.[!i]) )  (x.children) 
                                                    ) 
                |   _                           -> false
     // INTERFACES
         /// get structural hash code (called by LanguagePrimitives.GenericHash(_) )
        //interface IStructuralHash with
        //    member x.GetStructuralHashCode(_) =  
        override x.GetHashCode() =
                //ResizeArray.fold (fun output (child:Element) -> ((output) (**) +++ (*5*)(*71*)(*dimensione alfabeto*) (*+*)  child.address) (* % Int32.max_int*)) 1 x.children
                Seq.fold (fun output (child:Element) -> (output * (*5*)71(*dimensione alfabeto+1*) +  child.address) (* % Int32.max_int*)) 1 x.children
        //end 
        /// object comparator
        interface System.IComparable with
            member x.CompareTo(y:obj) = 
                match y with
                    |   :? LoopingSequence as ls    ->  if (x).Equals(ls) then 0 else 1
                    |   _                           ->  -1
        end
        
        /// clone each element in and return a new looping sequence
        override x.Clone() = 
            let newv = new LoopingSequence(new ResizeArray<_>(), x.parent);
            for c in x.children do
                newv.children.Add(c.Clone())
            done  
            //newv.putInNormalForm()
            newv :> Node
    end

/// represent term made by "looping and containment" operator
/// contains a membrane (=LoopingSequence) and a content (=Compartment)
and Loop = 
    class
        inherit Node //as base
     // FIELDS
        /// a membrane (a looping sequence)
        val membrane : LoopingSequence
        /// a content  (a compartment)
        val content : Compartment
          
     // CONSTRUCTORS
        /// make an empty membrane with empty content
        new(?p:Node Option) as x = { membrane = new LoopingSequence(new ResizeArray<_>()); content = new Compartment(None);  } 
                                    then 
                                        x.parent <- match p with None -> None | Some(pp) -> pp 
                                        x.membrane.parent <- Some(x :> Node);
                                        x.content.parent <- Some(x :> Node); 
        /// make an membrane with specified looping sequence with the specified contents
        new((seq: LoopingSequence), cont, ?p) as x = 
            seq.putInNormalForm(); 
            { membrane = seq; content = cont; }
            then 
                x.parent <- match p with None -> None | Some(pp) -> pp
                x.membrane.parent <- Some(x :> Node);
                x.content.parent <- Some(x :> Node); 
       
     // METHODS
        /// get the size (=number of element) of the membrane
        override l.Size : Int64 =   l.membrane.Size + l.content.Size
        
        /// get string representation
        override l.ToString() : string =
            //let parent_str = match l.parent with None -> "_" | Some(p) -> p.ToString()
            let output = new StringBuilder("loop(")
            if(l.content.children.Count <> 0) 
                then    output.Append(l.membrane.ToString() + ")[" + l.content.ToString() + "]") |> ignore
                else    output.Append(l.membrane.ToString() + ")") |> ignore
            output.ToString()// + " with parent " + parent_str
        
        /// get string representation with address translated to identifier
        override l.ToString(s: SymbolTable) : string =
            //let parent_str = match l.parent with None -> "_" | Some(p) -> p.ToString()
            let output = new StringBuilder(" loop( ")
            if(l.content.children.Count <> 0) 
                then    output.Append(l.membrane.ToString(s) + " ) [ " + l.content.ToString(s) + " ] ") |> ignore
                else    output.Append(l.membrane.ToString(s) + " ) ") |> ignore
            output.ToString()// + " with parent " + parent_str
        
        /// object equality
        override x.Equals(y:obj) = 
            match y with 
                | :? Loop as loop -> (x.membrane.Equals(loop.membrane))&&(x.content.Equals(loop.content) )
                | _-> false
      
      // INTERFACES
        /// structural equality
        //interface IStructuralHash with
        //    member x.GetStructuralHashCode(_) =  
        override x.GetHashCode() =
                LanguagePrimitives.GenericHash(x.content) ^^^ LanguagePrimitives.GenericHash(x.membrane)
        //end 
        /// object comparison
        interface System.IComparable with
            member x.CompareTo(y:obj) = 
                match y with 
                    | :? Loop as loop -> if x.Equals(loop) then 0 else 1
                    | _ ->   -1
        end
        /// clone the entire membrane and its content
        override x.Clone() = 
            let newv = new Loop((x.membrane.Clone() :?> LoopingSequence),( x.content.Clone():?> Compartment), x.parent);
            newv :> Node
    end

/// represent a Stochastic CLS Rule 
and Rule =                          
    class
     /// FIELDS
        /// identifier of the rule
        val name : string
        /// left hand side of the rule (the precondition of the rule)
        val left : Compartment
        /// right hand side of the rule (the post condition of the rule)
        val right : Compartment
        /// the number of variable in the lhs of the rules (as multiset)
        val mutable lhs_var_count : int
        /// the size in term of elements of the lhs of the rule
        val mutable lhs_size : Int64
        /// the number of variable in the rhs of the rules (as multiset)
        val mutable rhs_var_count : int
        /// the size in term of elements of the lhs of the rule
        val mutable rhs_size : Int64
        /// the rule is void able iff the left hand side can be instantiated to Epsilon (but E != loop(E)[E])
        val mutable voidable : bool 
        /// CSharpCodeExpressionEvaluator associated with the rule (with the specified rate function's source)
        val rate_function : CSharpCodeExpressionEvaluator
        /// source of the rate function (must be C# valid code)
        val mutable rate_function_source :string
        /// tell if the rule use an interpreted rate function or a simple float constant
        val mutable use_interpreted_rate_function :  bool
        /// eventually constant rate
        val mutable constant_rate : float
     /// CONSTRUCTOR
        new()  = { name = ""; left = new Compartment(None); right = new Compartment(None); rhs_var_count = 0; rhs_size = 0L; lhs_var_count = 0; lhs_size = 0L; voidable = false; rate_function = new CSharpCodeExpressionEvaluator(); rate_function_source = "" ; use_interpreted_rate_function= true; constant_rate=0.0; }    
        new(n, l, r(*, k*), rf:string, st:SymbolTable) as this = 
            { name = n; left = l; right = r;  rhs_var_count = 0; rhs_size = 0L; lhs_var_count = 0; lhs_size = 0L; voidable = false; rate_function = new CSharpCodeExpressionEvaluator(); rate_function_source = rf; use_interpreted_rate_function= true;constant_rate=0.0; }
                then
              this.lhs_size <- this.left.Size
              this.rhs_size <- this.right.Size
              let e,s,t = this.left.getVariables() in
              this.lhs_var_count <- e.Count + s.Count + t.Count
              let e,s,t = this.right.getVariables() in
              this.rhs_var_count <- e.Count + s.Count + t.Count
              this.voidable <- this.checkIfVoidable  
                             // FORCE TO EVALUATE DINAMICCALLY THE RATE FUNCTION
              //this.use_interpreted_rate_function  <- true
              //this.rate_function_source <- this.rate_function_source.Trim([|'#'|])
              
              if this.rate_function_source.[0] = '#'
                then
                    this.use_interpreted_rate_function  <- true
                    this.rate_function_source <- this.rate_function_source.Trim([|'#'|])
                else
                    this.use_interpreted_rate_function <- false
                    this.constant_rate <- System.Double.Parse(this.rate_function_source(*.Replace(".",",")*))
                    

               
     /// METHODS
        /// initialize the CSharpCodeExpressionEvaluator associated with the rule with the specified rate function's source
        /// [generate the the code of a class with a method with the specified source; compile it and create an instance of that method]
        member x.init(st) =
            if not x.use_interpreted_rate_function
                then x.constant_rate <- System.Double.Parse(x.rate_function_source.Replace(".",","))
                     Console.WriteLine(x.constant_rate.ToString())
                else
                    match x.rate_function.init(Rule.preprocessing_rate_function_source(x.rate_function_source ,st)) with
                        | false -> failwith ("Error in compilation of rate function for rule " + x.name + ": " + x.rate_function_source)
                        | _-> () 
            
        /// call the CSharpCodeExpressionEvaluator, associated and initialized for this rule,
        /// to obtain the rate constant associated with (give by binding) selected instantiation  of the rule         
        member x.getRate(b)  =
            //Console.WriteLine(x.rate_function.evalRateFunction(b).ToString() + " - " + x.constant_rate.ToString())
            if x.use_interpreted_rate_function 
                then x.rate_function.evalRateFunction(b)
                else x.constant_rate
            
            
        /// check if the rule is void able
        member x.checkIfVoidable : bool =
            let e,s,t = x.left.getVariables() 
            not(x.left.hasLoop) && (x.lhs_size = System.Convert.ToInt64 (s.Count + t.Count))
        
        member x.getReactantsSet(numberOfElements) = 
            let conc_left = x.left.getConcentrations(numberOfElements)
            let acc = new ResizeArray<_>()
            for i=0 to conc_left.Length - 1
                    do
                        if conc_left.[i] > 0L then acc.Add(i)
                        
                    done
            Set.ofSeq acc
        
        member x.getProductSet(numberOfElements) = 
            let conc_right = x.right.getConcentrations(numberOfElements)
            let acc = new ResizeArray<_>()
            
            for i=0 to conc_right.Length - 1
                    do
                        if conc_right.[i] > 0L then acc.Add(i)
                        
                    done
            Set.ofSeq acc
        
        /// pre-process the source code of rate function to obtain valid c# code
        /// [call a static method of SyntaxPreprocessor.PreprocessSource]
        static member preprocessing_rate_function_source((src:string), ( st : SymbolTable)) : string = 
            SyntaxPreprocessor.PreprocessSource(src,st)
            
        
        /// get string representation
        override r.ToString() : string =
           ("Rule = " + r.name + " : " + r.left.ToString() + " --" + r.rate_function_source + "--> " + r.right.ToString()) 
    end

/// represent an instance of the SCLS simulation problem
and Model =
    class
   /// FIELDS
     /// fixed set of rules  
        val rules : array<Rule>;
     /// the CLS term that represent the current state of the simulation
        val mutable term :  Compartment;
     /// the fixed symbol table
        val symbols : SymbolTable; 
     /// set of symbols that are variable
        val variables : addr Set
     /// set of user defined pattern to monitor
        val patterns : array<Pattern>
     /// set of element to not monitor
        val exclude : addr Set     
   /// CONSTRUCTOR
        new(((r):  array<Rule>),((p):  array<Pattern>),t,s: SymbolTable,vars,exclude: string Set) as x = 
            let exclude =
                Set.map (fun st -> s.inverse_get(st) ) exclude
                |>
                Set.filter (fun (o: _ Option) -> o.IsSome)
                |>
                Set.map (fun (o: _ Option) -> o.Value)
            {   rules =  r ;     term = t;  symbols = s;  variables=vars; patterns = p; exclude = exclude;   }
            then
                (
                if Model.checkForWellFormedRules(x.rules) 
                    then    ()
                    else    (failwith "Not Well Formed Rules !")
                )
   /// METHODS
        static member checkForWellFormedRules(rs: Rule array): bool = 
            let wellFormed = ref true
            let notFreshVariableRight = (
                fun (r:Rule) -> 
                    /// check if there are not variables on the right that are not on the left
                    let e_rs_variable, s_rs_variable, t_rs_variable = r.right.getVariables()
                    let e_ls_variable, s_ls_variable, t_ls_variable = r.left.getVariables()

                    (Set.ofSeq e_rs_variable).IsSubsetOf (Set.ofSeq e_ls_variable) &&
                    (Set.ofSeq s_rs_variable).IsSubsetOf (Set.ofSeq s_ls_variable) &&
                    (Set.ofSeq t_rs_variable).IsSubsetOf (Set.ofSeq t_ls_variable)     
                )
            Array.forall notFreshVariableRight rs
        
        /// get string representation  
        override m.ToString() : string =
           let rulesOutput = new StringBuilder()
           Array.iter (fun (rule:Rule) -> rulesOutput.Append("\n"+ rule.ToString():string) |> ignore) m.rules
           let patternsOutput = new StringBuilder()
           Array.iter (fun (p:Pattern) -> rulesOutput.Append("\n"+ p.ToString():string) |> ignore) m.patterns
           ("SCLS Model\n----RLES :"+ rulesOutput.ToString() 
           + "\n----TERM :\n" + m.term.ToString() 
           + "\n----SYMBOLS :\n" + m.symbols.ToString() 
           + "\n----PATTERN :\n" + patternsOutput.ToString() 
           )
    end

/// a bindings like seen by nfa_leaves_matcher
and nfa_bindings = int list Option array
    
/// represent a binding (a bind Option for each symbol)
and bindings = 
        class
        /// the array of bind Option : a bind Option for each symbols
        val mutable bind : (Node Option array) 
     // CONSTRUCTORS
        /// create an empty bindings (each symbol had bind None)
        //[<OverloadID("ctor1")>]
        new(size:int) = { bind = Array.create size None }
        /// create a bindings starting from an nfa_bindings
        //[<OverloadID("ctor2")>]
        new(l:nfa_bindings) as x = //here size = l.length but can not declare 2 constructor with 1 parameter 
            {bind = Array.create l.Length None} 
            then    
            (
                /// transform the bindings 
                /// like seen by nfa (=map from identifier(=address) to list of elements)
                /// to bindings (=map from identifier to Node)
                /// creating a new sequence for each list of elements
                let f = 
                    fun (intlsto:int list Option) ->
                    match intlsto with
                        | Some(intlst) ->
                            let rval = new Sequence(new ResizeArray<_>())
                            for adr in intlst do
                                rval.AddChild(new ConstantElement(adr))
                            done
                            Some((rval:>Node))
                        | None -> None 
                x.bind <- Array.map f l
            )
      // METHODS
        /// get the length of the bindings (=number of symbols)
        member  x.Length = x.bind.Length
        
        /// set or set the bind related to identifier (=address)
        member x.Item 
         with get(i) : Node Option   = if i < x.bind.Length 
                                            then x.bind.[i] 
                                            else failwith "Try to get a not know identifier"
         and  set(i) (v:Node Option) = if i < x.bind.Length then x.bind.[i] <- v else failwith "Try to set a not know identifier"
         //and  set(i) (v:Node Option) = if i < x.bind.Length then x.bind.[i] <- Some(v.Value.Clone()) else failwith "Try to set a not know identifier"
        /// a bindings is empty iff do not have any active (=Some(_)) bind 
        member  x.isEmpty : bool =
            (Array.forall (fun (o:_ Option) -> not o.IsSome) x.bind) 
        
        /// merge two bindings;
        /// if not clash on some bind then return Some(merged bind) else return None
        static member mergeBindings(b1:bindings, b2:bindings) : bindings Option =
            let clash = ref false
            /// clone b1 and, if not met clash, add each bind of b1 to the new bindings
            let newb = new bindings(b1.Length)
            newb.bind <- Array.copy b2.bind
            for i = 0 to b1.Length-1 do
                if not !clash && not (b1.[i] = None)
                    then    match b2.[i] with
                                | None ->     newb.[i] <- b1.[i]
                                | _ -> if b2.[i]  = b1.[i] 
                                            then ()
                                            else  clash := true
            done
            if !clash 
                then None
                else Some(newb)
        
        /// fold left merge bindings on array of bindings; if met a clash return None
        static member mergeArrayOfBindings(bl : bindings array, size:int) : bindings Option = 
            if bl.Length = 0
                then Some(new bindings(size)) 
                else 
                    let clash = ref false
                    let symbols_count = bl.[0].Length
                    let newb = new bindings(symbols_count)
                    newb.bind <- Array.copy  bl.[0].bind
                    for i = 0 to symbols_count-1 do
                        for j=1 to bl.Length-1 do 
                            if not !clash && not (bl.[j].[i] = None)
                                then    match newb.[i] with
                                            | None ->     newb.[i] <- bl.[j].[i]
                                            | _ -> if newb.[i]  =  bl.[j].[i]
                                                        then ()
                                                        else  clash := true
                        done 
                    done
                    if !clash 
                        then None
                        else Some(newb) 

        /// compute the list of bindings results of the Cartesian product of the merge of each
        /// bindings of each list; if there are not any valid bindings then return None
        static member mergeBindingsList((bl1:bindings list),(bl2:bindings list)) : bindings list Option =
            let current_bindings_list = ref []
            let had_made_some_valid_bindings = ref false
            /// for each bindings in the first list
            for b1 in bl1 do
                /// for each bindings in the second list
                for b2 in bl2 do
                    match bindings.mergeBindings(b1, b2) with
                        // add only the valid merged bindings
                        |   Some(b) ->  if not !had_made_some_valid_bindings then had_made_some_valid_bindings := true
                                        current_bindings_list := b :: !current_bindings_list
                        |   _ -> ()
                done
            done  
            if !had_made_some_valid_bindings 
                then    Some(!current_bindings_list)
                /// if had made only clash then return None
                else    None 
        
        /// fold left on the array the merge of bindings list
        /// (fold left the Cartesian product of merge_bindings)
        static member mergeArrayOfBindingsList(bla:bindings list array) : bindings list Option =
            match bla.Length with
                |   0 -> None
                |   1 -> Some(bla.[0])
                |   l ->
                    let mutable curr =  Some(bla.[0])
                    for i = 1 to l - 1 do
                        curr <- 
                            match curr with
                                |   None -> None
                                |   Some(bl) -> 
                                                match bindings.mergeBindingsList(bl, bla.[i]) with
                                                    |   None -> None
                                                    |   Some(mbl) -> Some(mbl)
                    done 
                    curr
        
        /// get string representation
        override x.ToString() =
            let out = new StringBuilder("[")
            Array.iteri ( fun i (e:Node Option) ->
                match e with
                    | None -> ()
                    | Some(n) -> out.Append("(" + i.ToString() + ":=" + n.ToString() + ")") |> ignore ) x.bind
            out.Append("]").ToString()
        end
/// represent an user defined pattern (what the user want to monitor)        
and Pattern =
    class
        val name : string
        val p : Compartment
        /// source of the rate function (must be C# valid code)
        val rate_function_source :string
        new(p,name,src:string) =
            let src = if src.Length = 0 then "1" else src 
            {p = p; name = name; rate_function_source = src; }
        /// get string representation
        override r.ToString() : string =
           ("Pattern = " + r.name + " : " + r.p.ToString() + " - src : " + r.rate_function_source) 
    end
 
namespace SCLS.Occurrences     

open SCLS  
open System.Collections.Generic;
open System.Text;
    
module Occurrences =
    begin
    
        /// count the number of occurrences of each symbols in what in the binding b for each where variable symbols
        let occ(what:int array, where:int array, b:bindings)  =
            let acc = ref 0L
            for w in where do
                match b.[w] with 
                    | None -> ()
                    | Some(n) -> 
                        match n with
                            | :? Sequence as s -> acc := !acc + s.count(what)
                            | :? Compartment as c -> acc := !acc + c.count(what)
                            | :? Loop as l -> acc := !acc + l.membrane.count(what) + l.content.count(what)
                            | _ -> () 
            done
            !acc
            
        /// translate the call by identifiers in a call by symbols
        let preprocess_parameter(what:string, s:SCLS.SymbolTable) :  string =
            
                let what_array =     what.Split([|' '|])
                                    |> Seq.map (fun (ide:string) -> match s.inverse_get(ide) with Some(addr) -> addr.ToString() | None -> "-1")
                                    |> Set.ofSeq
             
                if what_array.Count > 0 
                    then 
                        let sb = new StringBuilder()
                        for w in what_array do
                            sb.Append(w.ToString() + ", ") |> ignore
                        done
                        sb.Remove(sb.Length-2,2) |> ignore
                        sb.ToString()
                else ""       
               
        
                    
    end   
 

