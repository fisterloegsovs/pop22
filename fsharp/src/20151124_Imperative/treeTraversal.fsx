// Examples of binary tree traversals

// A binary tree. Leaves store no information
type BinTree<'a> =
  | Leaf
  | Node of BinTree<'a>*'a*BinTree<'a>

// A recursive pre-order traversal functional style
let rec preTraversal = function
  | Leaf -> []
  | Node (left, b, right) -> b :: (preTraversal left) @ (preTraversal right)

// A recursive pre-order traversal imperative style
let rec impPreTraversal f = function
  | Leaf -> ()
  | Node (left, b, right) -> f b; impPreTraversal f left; impPreTraversal f right

let tree = Node(Node(Node(Leaf, -3, Leaf), 0, Node(Leaf, 2,Leaf)),5,Node(Leaf, 7, Leaf))

printfn "Functional pre-order traversal: %A" (preTraversal tree)

let printNode x = printf "%d " x
printf "Imperative pre-order traversal: "
impPreTraversal printNode tree
printfn ""
