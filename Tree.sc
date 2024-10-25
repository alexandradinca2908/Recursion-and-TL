import scala.annotation.tailrec

trait Tree

case class EmptyTree() extends Tree
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
  def listTreeString(list: List[Tree]): String =
    val str = list.foldLeft("")((acc: String, t: Tree) => acc ++ treeToString(t) ++ ", ")
    str.dropRight(2)

  tree match
    case ID(str) => str
    case Node(list) => "(" ++ listTreeString(list) ++ ")"

def replace(tree: Tree, searchTree: Tree, replacement: Tree): Tree =
  tree match
    case ID(_) =>
      if (equal(tree, searchTree)) replacement
      else tree
    case Node(list) =>
      if (equal(tree, searchTree)) replacement
      else Node(list.map(t => replace(t, searchTree, replacement)))

val t4 : Tree = Node(List(Node(List(ID("a"), ID("Bb0"))), Node(List(ID("a"))), ID("ccc")))
val t5 : Tree = Node(List(Node(List(ID("a"), ID("Bb0"))), Node(List(ID("a"))), ID("Bb0")))

replace(t4, ID("a"), ID("REPLACED!"))
replace(t5, Node(List(ID("a"), ID("Bb0"))), Node(List()))

//  PARSER
//def charCount(str: List[Char], c: Char): Integer =
//  str match
//    case head :: tail =>
//      if (head == c) 1 + charCount(tail, c)
//      else charCount(tail, c)
//    case Nil => 0
//
//def checkParse(str: List[Char]): Boolean =
//  if (charCount(str, '(') != charCount(str, ')')) false
//  else true
//
//@tailrec
//def checkIllegalChars(str: List[Char]): Boolean =
//  str match
//    case head :: tail =>
//      if (isAlphaNum(head)
//        || head == '(' || head == ')'
//        || head == ',' || head == ' ')
//        checkIllegalChars(tail)
//      else false
//    case Nil => true
//
//def isAlphaNum(c: Char): Boolean =
//  if (c >= 'A' && c <= 'Z'
//    || c >= 'a' && c <= 'z'
//    || c >= '0' && c <= '9') true
//  else false
//
//def stringParser(str: List[Char]): (String, List[Char]) =
//  str match
//    case head :: tail =>
//      if (isAlphaNum(head)) {
//        val tuple = stringParser(tail)
//        (head.toString ++ tuple._1, tuple._2)
//      } else ("", str)
//    case Nil => ("", str)
//
//@tailrec
//def valParser(str: List[Char]): (Tree, List[Char]) =
//  str match
//    case head :: tail =>
//      head match
//        case '(' => (Node(List()), tail)
//        case ')' => (EmptyTree(), tail)
//        case ',' => valParser(tail)
//        case ' ' => valParser(tail)
//        case _ =>
//          val parsed = stringParser(str)
//          (ID(parsed._1), parsed._2)
//    case Nil => (EmptyTree(), Nil)
//
//def parser(str: List[Char]): Tree =
//  if (!(checkParse(str) && checkIllegalChars(str)))
//    print("Could not parse; check parenthesis or illegal chars")
//    EmptyTree()
//  else
//      ???
//
//val str1 = "((a, bb), cc, (dd, e, ()))"
//val str2 = str1.drop(0).dropRight(0)
//
//str1.drop(0).dropRight(0).split(", ").toList

//val l = List('a', 'b', 'c', 'd', ',')
//stringParser(l)