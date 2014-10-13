package fpinscala.datastructures

import fpinscala.datastructures.List._

object main extends App {

  val x = List(1, 2, 3, 4, 5)
  val y = List(6, 7, 8, 9)

  val t1 = Branch(Branch(Leaf(1), Leaf(10)), Branch(Leaf(2), Leaf(3)))
  println(Tree.sum(t1))
  println(Tree.max(t1))
}
