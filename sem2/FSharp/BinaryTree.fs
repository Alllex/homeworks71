(*
    Problem: BinaryTrees
    Author: Alex Semin
    Place: Math-Mech SPbU 171
    Time:  05.2013
*)

type BinaryTree<'T> =
    | Node of (BinaryTree<'T> * 'T * BinaryTree<'T>)
    | Leaf of 'T
    | Empty

let count tree =
    let rec count =
        function
        | Empty -> 0
        | Leaf _ -> 1
        | Node (left, _, right) -> 1 + count left + count right
    count tree

let size tree =
    let rec sizeK (k:int -> 'b) =
        function 
        | Empty -> k 0
        | Leaf _ -> k 1
        | Node(l, _, r) -> sizeK 
                            (fun size_l -> 
                                  sizeK (fun size_r -> 
                                           k (size_l + size_r + 1)
                                        ) r
                            ) l
    sizeK (printfn "%A") tree

    
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
    
let getMax tree = 
  let rec getMax = 
    function
    | Empty -> 0
    | Leaf x -> x
    | Node (_, v, r) -> if r = Empty then v else getMax r
  getMax tree
  
let getMin tree = 
  let rec getMin = 
    function
    | Empty -> 0
    | Leaf x -> x
    | Node (l, v, _) -> if l = Empty then v else getMin l
  getMin tree
  
let print tree = 

  let lenNum = (getMax tree).ToString().Length
  let lenNode = lenNum + 2
  let pipeShift = lenNode / 2
  let spacesToNum = lenNode - pipeShift + 1
  
  let spaces n =
      let rec spaces i s =
        if i = n then s
        else spaces (i + 1) (" " + s)
      spaces 0 ""  
      
  let norm (n : int) =
    let countSpaces = lenNum - (n.ToString().Length)
    "(" + (spaces (countSpaces)) + n.ToString() + ")"
  
  let printPipeLines n =
    if n > 0 then printf "%s|" (spaces pipeShift)
    let rec pipe n =
      if n > 0 then printf "%s|" (spaces (lenNode + 1)); pipe (n - 1)
    pipe (n - 1)
  
  let rec print level = 
    function
    | Empty -> printfn ""
    | Leaf v -> printf "%s" (norm v)
    | Node(l, v, r) ->
      
      printf "%s" (norm v)
      if r <> Empty then printf "--"
      match r with
      | Empty -> ()
      | Leaf rv -> printfn "%s" (norm rv)
      | _ -> print (level + 1) r
      if l <> Empty then
        printPipeLines (level + 1)
        printfn ""
        printPipeLines (level)
        if level > 0 then printf "%s" (spaces (spacesToNum))
      match l with
      | Empty -> ()
      | Leaf lv -> 
        printfn "%s" (norm lv)
      | _ -> print level l
    
  printfn "\nBinary Tree:"
  print 0 tree
  printfn ""
  
  
let t = insertList [7; 5; 10; 2; 3; 1; 6; 8; 12; 9; 11; 20; 17; 19; 95; 50; 25; 75; 90; 96; 97; 99] Empty
printfn "%A\n" t
print t

let tt' = insertList (500::100::[501..510]) Empty
let tt = insertList ([101..110]) tt'
print tt


