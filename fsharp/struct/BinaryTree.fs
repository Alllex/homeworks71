(*
    Problem: BinaryTrees
    Author: Alex Semin
    Place: Math-Mech SPbU 171
    Time:  04.2013
*)

type BinaryTree<'T> =
    | Node of (BinaryTree<'T> * 'T * BinaryTree<'T>)
    | Leaf of 'T
    | Empty
    
let rec insert x =
    function
    | Empty -> Leaf x
    | Leaf l when x > l -> Node(Empty, l, Leaf x)
    | Leaf l -> Node(Leaf x, l, Empty)
    | Node(l, v, r) when x > v -> Node(l, v, insert x r)
    | Node(l, v, r) -> Node(insert x l, v, r)
    
let rec exists x =
    function
    | Empty -> false
    | Leaf l -> l = x
    | Node(l, v, r) -> exists x l || exists x r || v = x
    
let rec insertList list tree = 
    match list with
    | [] -> tree
    | h::t -> insertList t (insert h tree)
    
let rec remove x =
    function
    | Empty -> Empty
    | Leaf l when x = l -> Empty
    | Leaf l -> Leaf l
    | Node(left, value, right) when x <> value -> 
        let newLeft = remove x left
        let newRight = remove x right
        if (newLeft = Empty && newRight = Empty) then Leaf value
                                                 else Node(newLeft, value, newRight)
    | Node(left, value, right) ->
        let rec merge left right =
            match left, right with
            | (Empty, _) -> right
            | (_, Empty) -> left
            | (Leaf l1, Leaf l2) -> Node(left, l2, Empty)
            | (_, Node(l, v, r)) -> Node(left, v, merge l r)
            | (Node(l, v, r), _) -> Node(merge l r, v, right)
        merge left right
    
let rec removeList list tree = 
    match list with
    | [] -> tree
    | h::t -> removeList t (remove h tree)
    
    

let depth tree =
    let rec depth d = 
        function
        | Empty -> d 
        | Leaf _ -> d + 1 
        | Node(l, _, r) -> max (depth (d+1) l) (depth (d+1) r)
    depth 0 tree
    
printfn "Depth test - %A" (depth (insertList [1..5] Empty))
    
let print tree =
    let rec printl level tl =
        function
        | Leaf l when level = tl -> printf "%A " l
        | Node(l, v, r) -> if level < tl then 
                             printl (level + 1) tl l
                             printl (level + 1) tl r
                           else
                             printf "%A " v 
        | _ -> ()     
    let d = depth tree
    for i in 1..d do
         printl i d tree
         printfn ""
    tree
        
printfn "%A" (print (insertList [4;2;3;1;5] Empty))

let test = (removeList [4;1001;2;-9;1;5;7] (insertList [1..5] Empty) = Leaf 3)
           &&
           (removeList [4;1001;2;3;1;5;7] (insertList [1..5] Empty) = Empty)
           &&
           (removeList [4;1001;2;3;1;5;7] (insertList [2;5;4;1;3] Empty) = Empty)
      

printfn "%A" test

printfn "%A" (insertList [2;5;4;1;3] Empty)
  

