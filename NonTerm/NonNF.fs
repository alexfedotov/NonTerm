module NonNF

type transition = { inp:string; dest:int }
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

let matchA' (s:string) (a:string list) (b:string list):string =
    if      List.exists (fun x -> s = x) b          then "b" + s
    elif    List.exists (fun x -> s + "$" = x) b    then ("b" + s + "$")
    elif    List.exists (fun x -> s = x) a          then "a" + s
    else    "e1"

let rec matchA (s:string) (a:string list) (b:string list) (non:string list):string = 
    if      List.exists (fun x -> s.Substring(0, (String.length s) - 1) = x) non then s.Substring(0, (String.length s)-1)
    elif    List.exists (fun x -> s = x) a  then "a" + s
    elif    String.length s > 1             then matchA (s.Substring(1)) a b non
    else    "e1"

let rec matchB (s:string) (a:string list) (b:string list) (non:string list) (nonPerm:string list):string =
    if String.length s <= 1 then
        if      List.exists (fun x -> s.Substring(0, (String.length s) - 1) = x) nonPerm then s.Substring(0, (String.length s)-1)
        elif    List.exists (fun x -> s = x) b then "b" + s
        else    "e1"
    else
        if      List.exists (fun x -> s + "$" = x) b    then ("b" + s + "$")
        elif    s.[String.length s - 2] = '$'           then "b" + s.Substring(0, (String.length s) - 1)
        elif    String.exists (fun c -> c = '$') s      then matchB (s.Split('$').[0] + "$" + s.Split('$').[1].Substring(1)) a b non nonPerm
        elif    List.exists (fun x -> s = x) a          then "a" + s
        else    "e1"

let rec checkPref (s:string) (pref:string):bool =
    match s with
    | ""    -> false
    | _ when s = pref -> true
    | _ -> checkPref (s.Substring(0, ((String.length s) - 1))) pref

let build (lhses:string list):state list = 

    let alph = getAlphabet (lhses)

    let nonNFPerm = getNonNFPerm(lhses)

    let labA = getLabels(lhses)

    let labB = getLabels(nonNFPerm)

    []