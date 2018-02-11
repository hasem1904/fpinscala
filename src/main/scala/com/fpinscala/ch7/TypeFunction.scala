package com.fpinscala.ch7

import java.util.concurrent.{ExecutorService, _}

/**
  * Created by havard on 3/10/17.
  */

trait Par2 {
  /**
    * Par now represents a function that needs an ExecutorService and returns a Future[A].
    * The creation of the Future[A], doesn't actually happen until the ExecutorService is provided.
    *
    * Container type Par[A]
    * Legislate the existence of the function we need
    */
  type Par[A] = ExecutorService => Future[A]

  /**
    * Creates a computation that immediately results in a value a:
    * Promotes a constant value to a parallel computation.
    *
    * For taking an unevaluated A and returning a computation that might evaluate in a separate thread.
    * It is called unit because in a sense it creates a unit of parallelism that just wraps a single value.
    *
    * @param a
    * @tparam A
    * @return
    */
  def unit[A](a: => A): Par[A]

  /**
    * Wraps the expression a for concurrent evaluation by run:
    * Wraps its unevaluated argument in a Par and marks it for concurrent
    * evaluation.
    * Derived combinator. For explicit forking.
    *
    * @param a
    * @tparam A
    * @return
    */
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  /**
    * Marks a computation for concurrent evaluation by run:
    * Marks a computation for concurrent evaluation.
    * The evaluation won't actually occur until forced by run function call.
    *
    * For explicit forking. Enabling parallelism explicitly under control of the programmer.
    *
    * @param a
    * @tparam A
    * @return
    */
  def fork[A](a: => Par[A]): Par[A]

  /**
    * For extracting the result value from a parallel computation.
    *
    * @param a
    * @tparam A
    * @return
    */
  //def get[A](a: Par[A]): A

  /**
    * Fully evaluates a given Par, spawning parallel computation as requested by fork and extracting the result value:
    * The run function is where the parallelism actually is implemented:
    * Extracts a value from Par by actually performing the computation.
    *
    * Par will now just be a pure data structure, run has to have some means of implementing the parallelism
    * whether it spawns new threads, delegates tasks to a thread pool, or uses other mechanism.
    *
    * @param a
    * @tparam A
    * @return
    */
  //def run[A](a: Par[A]): A
  def run[A](s: ExecutorService)(a: Par[A]): A

  /**
    * Combines the results of two parallel computations with a binary function:
    *
    * High order function for combining the result of two parallel computations.
    *
    * @param a
    * @param b
    * @param f
    * @tparam A
    * @tparam B
    * @tparam C
    * @return
    */
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): C

}

class TypeFunction {
  def sum(ints: Seq[Int]): Int =
    ints.foldLeft(0)((a, b) => a + b)

  def sumDivideAndConquer(ints: IndexedSeq[Int]): Int =
    if (ints.size <= 1)
    //headOption is a method defined in all collections in Scala.
      ints.headOption getOrElse 0
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      //Computes the left and right half in parallel
      //val sumL: Par.unit(sumDivideAndConquer2(l))
      //val sumR: Par.unit(sumDivideAndConquer2(r))

      //Extracts the results and sums them
      //Par.get(sumL) + Par.get(sumR)

      //Recursively sums both halves and adds the results together.
      sumDivideAndConquer(l) + sumDivideAndConquer(r)
    }

  def sumDivideAndConquer2(ints: IndexedSeq[Int]): Int =
    if (ints.size <= 1)
    //headOption is a method defined in all collections in Scala.
      ints.headOption getOrElse 0
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      //Recursively sums both halves and adds the results together.
      sumDivideAndConquer(l) + sumDivideAndConquer(r)
    }

}

object TypeFunctionsApp extends App {
  val ints = (1 to 15).toList
  val intsIndxSeq: IndexedSeq[Int] = 1 to 15
  val tf = new TypeFunction()
  println(s"> sum: ${tf.sum(ints)}")
  println(s"> sumDivideAndConquer: ${tf.sumDivideAndConquer(intsIndxSeq)}")
}