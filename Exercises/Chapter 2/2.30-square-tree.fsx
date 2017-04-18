type Tree =
    | Leaf of int
    | Node of Tree list

let square n = pown n 2

let rec squareTree t =
    match t with
    | Leaf value -> Leaf (square value)
    | Node children -> Node (children |> List.map squareTree)

#r @"C:\nuget_local\Unquote.3.1.1\lib\net45\Unquote.dll"
open Swensen.Unquote

let t = 
    Node([Leaf 1
          Node([Leaf 2; Node([Leaf 3; Leaf 4]); Leaf 5])
          Node([Leaf 6; Leaf 7])])

test <@ squareTree t = 
            Node([Leaf 1
                  Node([Leaf 4; Node([Leaf 9; Leaf 16]); Leaf 25])
                  Node([Leaf 36; Leaf 49])]) @>