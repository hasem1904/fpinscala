package com.fpinscala.ch2.exercise

import scala.annotation.tailrec

/**
  * A recursive function that get the n'th Fibonacci number.
  * The first two Fibonacci numbers are 0 and 1. The n'th number is always
  * the sum of the previous two. The sequence begins as follows:
  * E.g.: fib(8) = 0 + 1 + 1 + 2 + 3 + 5 + 8 + 13 => 8 + 13 = 21.
  * Created by havard on 1/14/17.
  */
class Exercise21 {
  def fib2(n: Int): Int = {
    @tailrec
    def loop(n: Int, prev: Int, cur: Int): Int =
      if (n == 0) prev
      else {
        println(s"loop($n, $prev, $cur)")
        loop(n - 1, cur, prev + cur)
      }
    loop(n, 0, 1)
  }
}

object Exercise21 {
  def main(args: Array[String]) {
    val ex = new Exercise21()
    var fibnum = 6
    println(s"Fibonacci number of $fibnum: ${ex.fib2(fibnum)}")
    fibnum = 8
    println(s"Fibonacci number of $fibnum: ${ex.fib2(fibnum)}")
  }
}
