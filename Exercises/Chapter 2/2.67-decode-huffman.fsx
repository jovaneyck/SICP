type Bit = O | I
type Leaf<'t> = {symbol : 't; weight : int}
type Node<'t> when 't : comparison = 
    {
        left : Tree<'t>
        right : Tree<'t>
        symbols : 't Set
        weight : int
    }
and
    Tree<'t> when 't : comparison = //Static types win: cannot have an "empty" tree
    | Leaf of Leaf<'t>
    | Node of Node<'t>
    
let makeLeaf symbol weight = Leaf {symbol = symbol; weight = weight}

let rec symbols =
    function
    | Leaf l -> Set.singleton l.symbol
    | Node {left = l; right = r} -> Set.union (symbols l) (symbols r)

let rec weight =
    function
    | Leaf {weight = w} -> w
    | Node {left = l; right = r} -> weight l + weight r

let makeCodeTree left right =
    Node <|
    {
        left = left
        right = right
        symbols = Set.union (symbols left) (symbols right)
        weight = weight left + weight right
    }

let decode bits tree = 
    match tree with
    | Leaf l -> failwithf "Cannot decode based on a leaf node (%A)" l //bug in Racket code found here because of static types?
    | Node rootNode ->
        let chooseBranch bit branch =
            match bit with
            | O -> branch.left
            | I -> branch.right
        let rec decodeOne bits currentBranch =
            match bits with
            | [] -> []
            | bit :: rest ->
                let nextBranch = chooseBranch bit currentBranch
                match nextBranch with
                | Leaf l -> l.symbol :: (decodeOne rest rootNode)
                | Node n -> decodeOne rest n
        decodeOne bits rootNode

let rec encodeSymbol symbol t =
    match t with
    | Leaf l -> failwithf "Cannot encode a symbol using a leaf (%A)" l //bug in Racket code found here because of static types?
    | Node root ->
        let {left = l; right = r} = root
        match l,r with
        | Leaf leaf, _ when leaf.symbol = symbol -> [O]
        | _ , Leaf leaf when leaf.symbol = symbol -> [I]
        | n, _ when Set.contains symbol (symbols n) -> O :: (encodeSymbol symbol n)
        | _, n when Set.contains symbol (symbols n) -> I :: (encodeSymbol symbol n)
        | _ -> failwithf "symbol %A not found in tree %A" symbol root

let rec encode message tree =
    match message with
    | [] -> []
    | s :: xs -> List.append (encodeSymbol s tree) (encode xs tree)

let rec adjoinSet el set =
    match set with
    | [] -> [el]
    | h :: t when (weight el) < (weight h) -> el :: set
    | h :: t -> h :: (adjoinSet el t)

let rec makeLeafSet pairs =
    match pairs with
    | [] -> []
    | (symbol, frequency) :: ps -> adjoinSet (makeLeaf symbol frequency) (makeLeafSet ps)

let rec successiveMerge leafSet =
    match leafSet with
    | [] -> failwith "Cannot merge an empty set"
    | [result] -> result
    | fst :: snd :: tail ->
        successiveMerge <| adjoinSet (makeCodeTree fst snd) tail

let generateHuffmanTree pairs =
    pairs
    |> makeLeafSet
    |> successiveMerge

#r @"C:\nuget_local\Unquote.3.1.1\lib\net45\Unquote.dll"
open Swensen.Unquote

//:(
let sampleTree = (makeCodeTree  (makeLeaf 'A' 4) 
                                        (makeCodeTree (makeLeaf 'B' 2) 
                                                      (makeCodeTree (makeLeaf 'D' 1) 
                                                                    (makeLeaf 'C' 1))))
let encodedMessage = [O; I; I; O; O; I; O; I; O; I; I; I; O]
let rawMessage = ['A';'D';'A';'B';'B';'C';'A']

let lyricTree =
    [("A", 2); ("NA", 16); ("BOOM", 1); ("SHA", 3); ("GET", 2); ("YIP", 9); ("JOB", 2); ("WAH", 1)]
    |> generateHuffmanTree
let lyrics = 
    "GET A JOB SHA NA NA NA NA NA NA NA NA GET A JOB SHA NA NA NA NA NA NA NA NA WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP SHA BOOM"
        .Split([| |])
        |> List.ofArray
let encodedLyrics = [I; I; I; I; I; I; I; O; O; I; I; I; I; O; I; I; I; O; O; O; O; O; O; O; O; O; I; I; I; I; I; I; I; O; O; I; I; I; I; O; I; I; I; O; O; O; O; O; O; O; O; O; I; I; O; I; O; I; O; I; O; I; O; I; O; I; O; I; O; I; O; I; O; I; O; I; I; I; O; I; I; O; I; I]

printf "Testing..."

test <@ weight <| makeLeaf 'A' 3 = 3 @>
test <@ weight <| makeCodeTree (makeLeaf 'a' 3) (makeLeaf 'b' 2) = 5 @>
test <@ symbols <| makeLeaf 'A' 1 = Set.singleton 'A' @>
test <@ symbols <| makeCodeTree (makeLeaf 'a' 3) (makeLeaf 'b' 2)
            = Set.ofList ['a'; 'b'] @>

test <@ rawMessage = decode encodedMessage sampleTree @>
test <@ encodedMessage = encode rawMessage sampleTree @>
test <@ rawMessage =decode (encode rawMessage sampleTree) sampleTree @>

test <@ makeLeafSet [('A', 8);('B', 3)]
            = [(makeLeaf 'B' 3); (makeLeaf 'A' 8)] @>

test <@ encode lyrics lyricTree = encodedLyrics @>
test <@ lyrics = (decode (encode lyrics lyricTree) lyricTree) @>
printfn "Done"