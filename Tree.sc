trait Tree

case class Node(tree: List[Tree]) extends Tree
case class ID(str: String) extends Tree

val t1 : Tree = Node(List(ID("abc")))
val t2 : Tree = Node(List(Node(List(ID("a"), ID("Bb0"))), Node(List(ID("this")))))
val t3 : Tree = Node(List(Node(List(ID("a"), ID("Bb0"))), Node(List(ID("this"))), ID("ccc")))

def equal(tree1: Tree, tree2: Tree): Boolean =
  (tree1, tree2) match
    //  2 ID's must match by string
    case (ID(s1), ID(s2)) => s1.equals(s2)
    //  2 different trees can't match
    case (ID, Node) => false
    case (Node, ID) => false
    //  2 nodes must have the same amount of children

    //  If that's the case: zip the children and compare them with map;
    //  The result is a list of booleans which we fold into one bool value
    case (Node(l1), Node(l2)) =>
      if (l1.length != l2.length) false
      else l1.zip(l2)
        .map(e => equal(e._1, e._2))
        .foldLeft(true)((acc: Boolean, x: Boolean) => acc & x)

def treeToString(tree: Tree): String =
  def listTreeString(list: List[Tree]): String =
    val str = list.foldLeft("")((acc: String, t: Tree) => acc ++ treeToString(t) ++ ", ")
    str.dropRight(2)

  tree match
    case ID(str) => str
    case Node(list) => "(" ++ listTreeString(list) ++ ")"

treeToString(t3)