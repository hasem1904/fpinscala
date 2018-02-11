package com.fpinscala.ch2

import scala.annotation.tailrec

/**
  * Created by havard on 1/13/17.
  */
object MyModule {
  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  def factorial(n: Int): Int = {
    //Auxiliary method.
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n - 1, n * acc)
    //Initial call...
    go(n, 1)
  }

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  private def formatFactorial(x: Int) = {
    val msg = "The factorial of %d is %d"
    msg.format(x, factorial(x))
  }

  private def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d"
    msg.format(name, n, f(n))
  }

  //findFirst: Looping by recursion
  def findFirst(ss: Array[String], key: String): Int = {
    //Auxiliary method for looping by recursion...
    def loop(n: Int): Int = {
      if (n > ss.length) -1
      else if (ss(n) == key) n
      else loop(n + 1)
    }
    loop(0)
  }

  //Polymorphic version of findFirst
  def findFirst[A](xs: Array[A], p: A => Boolean): Int = {
    //Auxiliary method for looping by recursion...
    @tailrec
    def loop(n: Int): Int = {
      if (n > xs.length) -1
      else if (p(xs(n))) n
      else loop(n + 1)
    }
    loop(0)
  }

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C =
  //(b: B) => f(a, b)
    b => f(a, b)

  //Exercise: 2.1
  def fib(n: Int): Int = {
    @tailrec
    def loop(n: Int, prev: Int, cur: Int): Int =
      if (n == 0) prev
      else {
        println(s"loop($n, $prev, $cur)")
        loop(n - 1, cur, prev + cur)
      }
    loop(n, 0, 1)
  }

  //Exercise: 2.2
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def loop(n: Int): Boolean =
      if (n >= as.length - 1) true
      else if (ordered(as(n), as(n + 1))) false
      else loop(n + 1)
    loop(0)
  }

  /**
    * Exercise: 2.3
    *
    * The curry function is a High Order Function (HOF) that takes an input function 'f'.
    * The curry function takes functions parameters of type A,B and C.
    * The curry function is a HOF, that takes an input function of type A and B and produces/results in a type C.
    * The curry function then returns a function that takes a type A and produces/results in a function that takes
    * an input parameter A and returns a new function that takes an input parameter B and produces/results in a type C
    * (that matches the input function f to the curry function)
    *
    * @param f is the curry's HOF input function.
    * @tparam A type parameter.
    * @tparam B type parameter.
    * @tparam C type parameter.
    * @return a new function that takes an input of type A and result in a new function that takes an input of type B and results in a type C.
    */
  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    a => b => f(a, b)

  /**
    * Exercise 2.4
    * Implement a function uncurry, which reverses the transformation of the curry function.
    * Note that since => associates to the right, A => (B => C) can be written as A => B => C.
    *
    * @param f is the uncurry's HOF input function.
    * @tparam A type parameter.
    * @tparam B type parameter.
    * @tparam C type parameter.
    * @return a new function that takes an input parameter of type A and B an results in a type C.
    */
  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
  //Note: Anonymous function with input param of type A and B, noted: (a, b)
  //f: A => B => C is equal to: f(a) => f(b) => C
    (a, b) => f(a)(b)

  /**
    * The compose is a function which feeds the output of one function to the input of another function.
    *
    * @param f is the compose HOF input function.
    * @param g is the compose HOF input function.
    * @tparam A type parameter.
    * @tparam B type parameter.
    * @tparam C type parameter.
    * @return
    */
  def compose[A, B, C](f: B => C, g: A => B): A => C =
    //Note: Anonymous function with input param of type A, noted: a
    //Anonymous function with input param A and results/produces a C, noted: g(a) => B, and f(g(a)) => C
    a => f(g(a))

  def main(args: Array[String]) {
    println(formatAbs(-42))
    println(formatFactorial(7))
    println(formatResult("absolute value", -42, abs))
    println(formatResult("factorial", 7, factorial))
    println(">findFirst: index of 9: " + findFirst(Array(7, 9, 13), (x: Int) => x == 9))
    val fibnum = 6
    println(s"Fibonacci number of $fibnum: ${fib(fibnum)}")

    val xs = Array(1, 3, 5, 6, 57)
    val ordered = (x: Int, y: Int) => x > y
    println(s"isSorted ${xs.mkString(",")}: ${isSorted(xs, ordered)}")
  }
}
