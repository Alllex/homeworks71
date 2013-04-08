(*
    Problem: Lists
    Author: Alex Semin
    Place: Math-Mech SPbU 171
    Time:  03.2013
*)

let rec push x =
    function
    | [] -> [x]
    | head::tail -> head::(push x tail)
    
let rec append l1 l2 =
    match l1 with
    | [] -> l2
    | head::tail -> head::(append tail l2)

let rec reverse = 
    function
    | [] -> []
    | head::tail -> push head (reverse tail)
    
let rec find pr =
    function
    | [] -> None
    | head::tail -> if pr head then Some head
                               else find pr tail

let rec map op list = List.foldBack (fun x acc -> (op x)::acc) list []

let test = (push 5 [1..4]) = [1..5]
           && (append [1..2] [3..5]) = [1..5]
           && (reverse [1..9]) = [9..-1..1]
           && (find (fun x -> x % 5 = 1) [2..9]) = Some 6
           && (map ((+) 2) [1..5]) = [3..7]
     
printfn "%A" test

