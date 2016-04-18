module NonNF

(*
 *TODOs:
 * - Refactor
 * - DFA minimizer
 *)

exception InnerError of string

type transition = { inp:char; dest:int }
type state = { stateID:int; transitions: transition list; isFinal:bool }

let getAlphabet (lhses:string list):char list = 

    let rec getAlphabet' (ls:string list):char list = 
        match ls with
        | []    -> []
        | x::xs -> [for s in x do yield s] @ getAlphabet' xs

    Set.toList (set (getAlphabet' lhses))

let getNonNFPerm (lhses:string list):string list = 

    let rec getPerm (ls:string) (res:string list):string list =
        match ls with
        | "" -> res
        | _  -> if (not (String.exists ((=) '$') ls) && ls.Length > 1) 
                then 
                    getPerm (ls.[1..] + "$" + char(ls.[0]).ToString()) (res @ [ls.[1..] + "$" + char(ls.[0]).ToString()]) 
                else 
                    if 
                        (ls.[1] = '$') 
                    then 
                        res 
                    else 
                        getPerm (ls.[1..] + char(ls.[0]).ToString()) (res @ [ls.[1..] + char(ls.[0]).ToString()])

    let rec getNonNFPerm' (ls:string list):string list =
        match ls with
        | []    -> []
        | x::xs -> getPerm x (getNonNFPerm' xs)

    Set.toList (set (getNonNFPerm' (List.filter (fun x -> String.length x > 1) lhses)))

let getLabels (lhses:string list):string list = 

    let rec getLabels' (ls:string list):string list =
        match ls with
        | []    ->  []
        | x::xs ->  if String.length x > 1
                    then
                        [for i in 1..(String.length x) do yield x.Substring(0,i)] @ getLabels' xs
                    else
                        x :: getLabels' xs
    
    let res = Set.toList (set (getLabels' lhses))
    List.filter (fun x -> not (List.exists (fun x' -> x' = x + "$") res)) res
 
let rec checkPref (s:string) (ps:string list):string =

    let rec checkPref' (s:string) (pref:string):bool =
        match s with
        | ""    -> false
        | _ when s = pref -> true
        | _ -> checkPref' (s.Substring(0, ((String.length s) - 1))) pref 

    match ps with
    | [] -> ""
    | x::xs when checkPref' s x -> x
    | _ -> checkPref s ps.Tail        

let matchA' (s:string) (a:string list) (b:string list) (non:string list) (nonPerm:string list):string =
    //if      List.exists (fun x -> s = x) non        then "a" + s
    if      List.exists (fun x -> s = x) b          then "b" + s
    elif    List.exists (fun x -> s + "$" = x) b    then ("b" + s + "$")
    elif    List.exists (fun x -> s = x) a          then "a" + s
    else    "e1"

let rec matchA (s:string) (a:string list) (b:string list) (non:string list) (nonPerm:string list):string = 
    if      List.exists (fun x -> s = x) a  then "a" + s
    elif    String.length s > 1             then matchA (s.Substring(1)) a b non nonPerm
    else    "e1"

let rec matchB (s:string) (a:string list) (b:string list) (non:string list) (nonPerm:string list):string =
    if String.length s <= 1 then
        //if      List.exists (fun (x:string) -> (s.Replace("$","") ) = x) non then "a" + (s.Replace ("$",""))
        if      List.exists (fun x -> s = x) b then "b" + s
        else    "e1"
    else
//        if      List.exists (fun (x:string) -> (s.Replace ("$","")) = x) non then "a" + (s.Replace ("$","")) 
        if      List.exists (fun x -> s = x) b          then "b" + s
        elif    List.exists (fun x -> s + "$" = x) b    then ("b" + s + "$")
        elif    s.[String.length s - 2] = '$'           then "b" + s.Substring(0, (String.length s) - 1)
        elif    String.exists (fun c -> c = '$') s      then
                                                            let s1 = s.Split('$').[0]
                                                            let s2 = s.Split('$').[1]
                                                            matchB (s1 + "$" + s2.Substring(1)) a b non nonPerm
        elif    checkPref (s.Replace ("$","")) a <> "" then "a" + checkPref (s.Replace ("$","")) a
//        elif    List.exists (fun (x:string) -> s = (x.Replace ("$",""))) a          then "a" + s
        else    "e1"

let makeStateMap (a:string list) (b:string list):Map<string,int> =
    let map = Map.empty.Add("e0", 0).Add("e1", 1)

    let rec addToMap (p:string) (ls:string list) (m:Map<string,int>):Map<string,int> =
        match ls with
        | [] -> m
        | x::xs -> addToMap p xs (m.Add(p + x, m.Count))

    addToMap "b" b (addToMap "a" a map)

let makeState (id:int) (l:string) (alph:char list) (a:string list) (b:string list) (non:string list) (nonPerm:string list) (m:Map<string,int>) f:state =
    let trans = [for i in alph do yield {inp = i; dest = Map.find (f (l + i.ToString()) a b non nonPerm) m}]
    {stateID = id; transitions = trans; isFinal = if (List.exists (fun x -> (l.Replace ("$","")) = x) non || List.exists (fun x -> l = x) nonPerm) then true else false}

let build (lhses:string list):state list = 

    let alph = getAlphabet (lhses)

    let nonNFPerm = getNonNFPerm(lhses)

    let labA = getLabels(lhses)

    let labB = getLabels(nonNFPerm)

    let m = makeStateMap labA labB

    [makeState 0 "" alph labA labB lhses nonNFPerm m matchA'] @ 
    [makeState 1 "" alph labA labB lhses nonNFPerm m matchA] @
    [for lab in labA do yield makeState (Map.find ("a" + lab) m) lab alph labA labB lhses nonNFPerm m matchA] @
    [for lab in labB do yield makeState (Map.find ("b" + lab) m) lab alph labA labB lhses nonNFPerm m matchB]

type redGroup = { destGroups:int list; states:state list }

let mooreReduce (s:state list):state list = 
    
    let g = [ List.filter (fun (x:state) -> x.isFinal = false) s; List.filter (fun (x:state) -> x.isFinal = true) s ]

    let split (s:state list):(state list) list =

        let rec getDest (t:transition list):int list = 
            match t with
            | [] -> []
            | x::xs -> [ x.dest ] @ getDest xs

        let rec formGroups (s:state list) (g:redGroup list):redGroup list =
            match s with
            | [] -> g
            | x::xs ->  if 
                            List.exists (fun y -> y.destGroups = getDest x.transitions) g 
                        then 
                            formGroups xs (List.map (fun z -> if z.destGroups = getDest x.transitions then { destGroups = z.destGroups; states = z.states @ [x] } else z) g) 
                        else 
                            formGroups xs (g @ [{ destGroups = getDest x.transitions; states = [x] }])

        let rec grToList (gr:redGroup list):(state list) list = 
            match gr with
            | [] -> [[]]
            | x::xs -> [x.states] @ grToList xs
            
        List.filter (fun x -> x <> []) (formGroups s [] |> grToList)

    let rec reduce (gs:(state list) list):(state list) list =
        
        let rec reduceStep (gs:(state list) list):(state list) list = 
            match gs with
            | [] -> []
            | x::xs -> split x @ reduceStep xs
        
        if gs <> reduceStep gs then reduce (reduceStep gs) else gs

    let rec getNewTrans (counter:int) (id:int) (states:(state list) list):int =
        let getNewTrans' (id:int) (s:state list):int = if List.exists (fun (x:state) -> x.stateID = id) s then id else -1
        
        match states with
        | [] -> raise (InnerError("State not found"))
        | x::xs -> if getNewTrans' id x <> -1 then id else getNewTrans (counter+1) id xs  

    let builReducedSt (id:int) (s:state list) (g:(state list) list):state =

        let rec buildReducedTr (from:int) (s:state list) (groups:(state list) list):transition list =
            match s with
            | [] -> []
            | x::xs -> (List.map (fun z -> { inp = z.inp; dest = getNewTrans 0 z.dest groups} ) x.transitions) @ buildReducedTr from xs groups
        { stateID = id; transitions = Set.toList (set (buildReducedTr id s g)); isFinal = true }

    let r = reduce g
    let newStates = [for i in 0..((List.length r) - 1) do yield builReducedSt 0 (r.[i]) r]
    newStates