package fpinscala.datastructures

import fpinscala.datastructures.List._

object main extends App {

  val x = List(1, 2, 3, 4, 5)
  val y = List(6, 7, 8, 9)
  println(x)
  println(foldLeftAppend(x, y))
  println(append(x, y))
  println(inc(x))
  println(filter(x)(_ == 3))
//  assert(drop(x, 6) == List(1, 2, 3, 4, 5))
//  assert(drop(x, 0) == List(1,2, 3, 4, 5))
//  assert(drop(x, 3) == List(4, 5))

}
