package com.fpinscala.ch3

import scala.annotation.tailrec

/**
  * Exercises related to FP in Scala.
  *
  * Created by havard on 1/15/17.
  */
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  private val ErrorMsg: String = "tail of empty list"

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 0.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

  //Utility function
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  //Test of foldRight
  def sum2(ns: List[Int]) = foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Int]) = foldRight(ns, 1)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`

  //Exercise: 3.1
  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  //Exercise: 3.2
  def tail(xs: List[Int]): List[Int] = xs match {
    case Nil => sys.error(ErrorMsg)
    case Cons(_, t) => t
  }

  //Exercise: 3.3
  def setHead[A](xs: List[A], h: A): Unit = xs match {
    case Nil => sys.error(ErrorMsg)
    case Cons(_, t) => Cons(h, t)
  }

  //Exercise: 3.4
  def drop[A](xs: List[A], n: Int): List[A] = {
    //Return list when all elements are dropped.
    if (n <= 0) xs
    else xs match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n - 1)
    }
  }

  //Exercise: 3.5
  def dropWhile[A](xs: List[A], f: A => Boolean): List[A] = xs match {
    //Using guard (if statement)
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => xs
  }

  //Exercise: 3.6
  def init[A](xs: List[A]): List[A] = xs match {
    case Nil => sys.error(ErrorMsg)
    case Cons(_, Nil) => Nil //Last element in list should be left out by returning an empty list.
    case Cons(h, t) => Cons(h, init(t)) //Otherwise build a list by adding head elements in front.
  }

  /**
    * No, this is not possible! The reason is because _before_ we ever call our function, `f`, we evaluate its argument,
    * which in the case of `foldRight` means traversing the list all the way to the end. We need _non-strict_ evaluation
    * to support early termination---we discuss this in chapter 5.
    */
  //Exercise: 3.7
  def product3(ds: List[Double]): Double = foldRight(ds, 1.0)(_ * _)

  def sum3(ds: List[Double]): Double = foldRight(ds, 0.0)(_ + _)

  /**
    * We get back the original list! Why is that? As we mentioned earlier, one way of thinking about what `foldRight` "does"
    * is it replaces the `Nil` constructor of the list with the `z` argument, and it replaces the `Cons` constructor with
    * the given function, `f`. If we just supply `Nil` for `z` and `Cons` for `f`, then we get back the input list.

    * foldRight(Cons(1, Cons(2, Cons(3, Nil))), Nil:List[Int])(Cons(_,_))
    * Cons(1, foldRight(Cons(2, Cons(3, Nil)), Nil:List[Int])(Cons(_,_)))
    * Cons(1, Cons(2, foldRight(Cons(3, Nil), Nil:List[Int])(Cons(_,_))))
    * Cons(1, Cons(2, Cons(3, foldRight(Nil, Nil:List[Int])(Cons(_,_)))))
    * Cons(1, Cons(2, Cons(3, Nil)))
    *
    * foldLeft(Cons(3,Cons(2,Cons(1,Nil)))), Nil:List[Int])((x, y) => Cons(y, x))
    * Cons(3, foldLeft(Cons(2, Cons(1, Nil)), Nil:List[Int])((x, y) => Cons(y, x))
    * Cons(3, Cons(2, foldLeft(Cons(1, Nil), Nil:List[Int])((x, y) => Cons(y, x))
    * Cons(3, Cons(2, Cons(1, foldLeft(Nil, Nil:List[Int])((x, y) => Cons(y, x))
    * Cons(3, Cons(2, Cons(1, Nil)))
    */
  //Exercise: 3.8
  val foldRightTest: List[Int] = foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _))
  val foldLeftTest: List[Int] = foldLeft(List(1, 2, 3), Nil: List[Int])((x, y) => Cons(y, x))

  //Exercise: 3.9
  def length[A](as: List[A]): Int = foldRight(as, 0)((_, acc) => acc + 1)

  //Exercise: 3.10
  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  //Exercise: 3.11
  def product4(ds: List[Double]): Double = foldLeft(ds, 1.0)(_ * _)

  def sum4(ds: List[Double]): Double = foldLeft(ds, 0.0)(_ + _)

  def length2[A](as: List[A]): Int = foldLeft(as, 0)((acc, _) => acc + 1)

  //Exercise: 3.12
  def reverse[A](as: List[A]): List[A] = foldLeft(as, Nil: List[A])((acc, h) => Cons(h, acc))

  //Exercise: 3.13
  //foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B
  def foldLeftViaFoldRight[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    foldRight(as, (b: B) => b)((a, g) => b => g(f(b, a)))(z)

  //foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B
  def foldRightViaFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((acc, h) => f(h, acc))

  //Exercise: 3.14
  def appendFoldRight[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)(Cons(_, _))

  def appendFoldLeft[A](a1: List[A], a2: List[A]): List[A] = foldLeft(a1, a2)((acc, xs) => Cons(xs, acc))

  //Exercise: 3.15
  def concat[A](as: List[List[A]]): List[A] =
  //foldLeft(as, Nil:List[A])((b,a) => append(a, b))
    foldRight(as, Nil: List[A])((as, bs) => append(as, bs))

  //Exercise: 3.16
  def transform(as: List[Int]): List[Int] =
    foldRight(as, Nil: List[Int])((h, t) => Cons(h + 1, t))

  //Exercise: 3.17
  def transformToString(as: List[Double]): List[String] =
    foldRight(as, Nil: List[String])((h, t) => Cons(h.toString(), t))

  //Exercise: 3.18
  def map[A, B](as: List[A])(f: A => B): List[B] =
    foldRight(as, Nil: List[B])((h, t) => Cons(f(h), t))

  //Exercise: 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A])((h, t) => if (f(h)) Cons(h, t) else t)

  //Exercise: 3.20: Flattens two list into one...
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
  //concat(map(as)(a =>f(a)))
    concat(map(as)(f))

  //Exercise: 3.21
  def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) List(a) else Nil)

  //Exercise: 3.22
  def zip(a: List[Int], b: List[Int]): List[Int] = (a, b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, zip(t1, t2))
  }

  //Exercise: 3.23
  def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = (a, b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }

  //Exercise: 3.24. Solution consists of two functions, startsWith and hasSubsequence.
  /**
    * There's nothing particularly bad about this implementation,
    * except that it's somewhat monolithic and easy to get wrong.
    * Where possible, we prefer to assemble functions like this using
    * combinations of other functions. It makes the code more obviously
    * correct and easier to read and understand. Notice that in this
    * implementation we need special purpose logic to break out of our
    * loops early. In Chapter 5 we'll discuss ways of composing functions
    * like this from simpler components, without giving up the efficiency
    * of having the resulting functions work in one pass over the data.

    * It's good to specify some properties about these functions.
    * For example, do you expect these expressions to be true?

    * (xs append ys) startsWith xs
    * xs startsWith Nil
    * (xs append ys append zs) hasSubsequence ys
    * xs hasSubsequence Nil
    */
  @tailrec
  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l, prefix) match {
    //Compare two head elements of two lists again each other.
    case (_, Nil) => true
    case (Cons(h, t), Cons(h2, t2)) if h == h2 => startsWith(t, t2)
    case _ => false
  }

  //Check if two lists has subsequence.
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => sub == Nil
    case _ if startsWith(sup, sub) => true
    case Cons(h, t) => hasSubsequence(t, sub)
  }
}

//Testing List methods...
object ListApp extends App {
    println(s"x:       \t\t$List.x")
    println(s"Init:    \t\t${List.init(List(1, 2, 3, 4))}")
    println(s"Sum2:    \t\t${List.sum2(List(1, 3, 5))}")
    println(s"Sum3:    \t\t${List.sum3(List(1.0, 3.0, 5.0))}")
    println(s"Sum4:    \t\t${List.sum4(List(1.0, 3.0, 5.0))}")
    println(s"Product2:\t\t${List.product2(List(1, 3, 5))}")
    println(s"Product3:\t\t${List.product3(List(0.0, 0.0, 0.0))}")
    println(s"Product4:\t\t${List.product4(List(1.0, 3.0, 5.0))}")
    println(s"foldRightTest:\t${List.foldRightTest}")
    println(s"foldLeftTest:\t${List.foldLeftTest}")
    println(s"length:\t\t\t${List.length(List(1, 2, 3))}")
    println(s"length2:\t\t${List.length2(List(1, 2, 3))}")
    println(s"reverse:\t\t${List.reverse(List(1, 2, 3))}")
    println(s"appendFoldRight: ${List.appendFoldRight(List(1, 2, 3), List(4))}")
    println(s"appendFoldRight: ${List.appendFoldLeft(List(1, 2, 3), List(4))}")
    println(s"concat:\t\t\t${List.concat(List(List(1, 2, 3), List(4, 5, 6)))}")
    println(s"transform:\t\t\t${List.transform(List(1, 2, 3))}")
    println(s"transformToString: ${List.transformToString(List(1.0, 2.0, 3.0))}")
    println(s"map:\t\t\t${List.map(List(1.0, 2.0, 3.0))(_.toString)}")
    println(s"filter:\t\t\t${List.filter(List(1.0, 2.0, 3.0))(_ > 2.0)}") //`(x) => x > 2.0`
    println(s"flatMap:\t\t${List.flatMap(List(1, 2, 3))(i => List(i, i))}")
    println(s"filterViaFlatMap:\t\t${List.filterViaFlatMap(List(1, 2, 3))(_ > 2)}")
    println(s"addParwise:\t\t${List.zip(List(1, 2, 3), List(4, 5, 6))}")
    println(s"zipWith:\t\t${List.zipWith(List(1, 2, 3), List(4, 5, 6))((a, b) => a + b)}")
    println(s"zipWith:\t\t${List.zipWith(List(1, 2, 3), List(4, 5, 6))((a, b) => a - b)}")
    println(s"hasSubsequence:\t${List.hasSubsequence(List(1, 2, 3, 4), List(1, 2))}")
    println(s"hasSubsequence:\t${List.hasSubsequence(List(1, 2, 3, 4), List(2, 3))}")
    println(s"hasSubsequence:\t${List.hasSubsequence(List(1, 2, 3, 4), List(5, 6))}")
}
