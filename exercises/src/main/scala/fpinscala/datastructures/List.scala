package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
case class Cons[+A](head: A, tail: List[A]) extends List[A] // Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  } 
  
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }
  
  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42 
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101 
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  
  def sum2(ns: List[Int]) = 
    foldRight(ns, 0)((x,y) => x + y)
  
  def product2(ns: List[Double]) = 
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil           => Nil
      case Cons(_, tail) => tail
    }

  def setHead[A](l: List[A], h: A): List[A] =
    l match {
      case Nil => Cons(h, Nil)
      case _   => Cons(h, l)
    }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else
      l match {
        case Nil => Nil
        case Cons(_, tail) => drop(tail, n - 1)
      }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(head, tail) if f(head) => dropWhile(tail, f)
      case _ => l
    }

  def myinit[A](l: List[A]): List[A] = {
    def loop(list: List[A], acc: List[A]): List[A] =
      list match {
        case Cons(h, Nil) => acc
        case Cons(h, t)   => loop(t, append(acc, Cons(h, Nil)))
      }
    loop(l, Nil)
  }

  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil            => sys.error("wth, why are u giving me empty list")
      case Cons(_, Nil)   => Nil
      case Cons(h, tail)  => Cons(h, init(tail))
    }
  }

  def length[A](l: List[A]): Int =
    l match {
      case Nil        => 0
      case Cons(_, t) => 1 + length(t)
    }

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
     }

  /*foldLeft(Cons(1, Cons(2, Cons(3, Cons(4, Nil)))), z)(f)
  *
  * 1.foldLeft(Cons(2, Cons(3, Cons(4, Nil))), z1)(f)
  * foldLeft(Cons(3, Cons(4, Nil)), z12)(f)
  * foldLeft(Cons(4, Nil)), z123)(f)
  * foldLeft(Nil), z1234)(f)
  * */


  def mySum(l: List[Int]) =
    foldLeft(l, 0)(_ + _)

  def myProd(l: List[Int]) =
    foldLeft(l, 1)(_ * _)

  def myLength[A](l: List[A]) =
    foldLeft(l, 0)((m, n) => m + 1)

  def myRevers[A](l: List[A]) = {
    val cons: List[A] = List()
    foldLeft(l, cons)((acc, item) => Cons(item, acc))
  }

  def foldRightAppend[A](a1: List[A], a2: List[A]): List[A] = {
    foldRight(a1, a2)((elem, acc) => Cons(elem, acc))
  }

  def foldLeftAppend[A](a1: List[A], a2: List[A]): List[A] = {
    foldLeft(myRevers(a1), a2)((acc, elem) => Cons(elem, acc))
  }

  def concat[A](l: List[List[A]]): List[A] = {
    foldRight(l, List[A]())(append)
  }

  def map[A, B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil: List[B])((a, b) => Cons(f(a), b))

  def inc(l: List[Int]) = map(l)(_ + 1)

  def doubleToString(l: List[Double]): List[String] = map(l)(_.toString)

  def filter[A](l: List[A])(f: (A => Boolean)): List[A] =
    l match {
      case Cons(x, xs) if f(x) => Cons(x, filter(xs)(f))
      case Cons(x, xs) => filter(xs)(f)
      case Nil => Nil
    }

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    concat(map(as)(f))

  def filterFlatMap[A](l: List[A])(f: (A => Boolean)): List[A] = {
    flatMap(l)((a) => if (f(a)) List(a) else Nil)
  }

  def sumLists(l: List[Int], m: List[Int]): List[Int] = (l, m) match {
    case (_, Nil)                   => Nil
    case (Nil, _)                   => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(x +y, sumLists(xs, ys))
  }

  def zipWith[A](l: List[A], m: List[A])(f: (A, A) => A): List[A] = (l, m) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }

}