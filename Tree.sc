import scala.annotation.tailrec

trait Tree

case class EmptyTree() extends Tree
case class Node(tree: List[Tree]) extends Tree
case class ID(str: String) extends Tree

def equal(tree1: Tree, tree2: Tree): Boolean =
  (tree1, tree2) match
    //  2 ID's must match by string
    case (ID(s1), ID(s2)) => s1.equals(s2)

    //  2 different trees can't match
    case (ID(_), Node(_)) => false
    case (Node(_), ID(_)) => false

    //  2 nodes must have the same amount of children

    //  If that's the case: zip the children and compare them with map;
    //  The result is a list of booleans which we fold into one bool value
    case (Node(l1), Node(l2)) =>
      if (l1.length != l2.length) false
      else l1.zip(l2)
        .map(e => equal(e._1, e._2))
        .foldLeft(true)((acc: Boolean, x: Boolean) => acc & x)

def treeToString(tree: Tree): String =
  //  Receive a list of trees (usually the contents of a node)
  //  Traverse the list using fold and recursively call main function
  //  to transform all trees
  def listTreeString(list: List[Tree]): String =
    val str = list.foldLeft("")((acc: String, t: Tree) => acc ++ treeToString(t) ++ ", ")
    str.dropRight(2)

  tree match
    case ID(str) => str
    case Node(list) => "(" ++ listTreeString(list) ++ ")"

def replace(tree: Tree, searchTree: Tree, replacement: Tree): Tree =
  tree match
    //  When an ID is found, we can either replace it or return
    case ID(_) =>
      if (equal(tree, searchTree)) replacement
      else tree
    //  When a Node is found, we can either replace it or check all
    //  its children recursively
    case Node(list) =>
      if (equal(tree, searchTree)) replacement
      else Node(list.map(t => replace(t, searchTree, replacement)))