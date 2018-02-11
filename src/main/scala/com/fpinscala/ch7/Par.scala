package com.fpinscala.ch7

import java.util.concurrent.{ExecutorService, _}

/**
  * Created by havard on 3/10/17.
  */
object Par {
  type Par[A] = ExecutorService => Future[A]

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] =
    map2(parList, unit(()))((a, _) => a.sorted)

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = (es: ExecutorService) => {
    val af = a(es)
    val bf = b(es)
    UnitFuture(f(af.get(), bf.get()))
  }

  def sortPar2(parList: Par[List[Int]]): Par[List[Int]] =
    map(parList)(_.sorted)

  /**
    * Lift function of type A => B to become a function that takes Par[A] and returns Par[B].
    * We can map any function over Par:
    *
    * @param pa
    * @param f
    * @tparam A
    * @tparam B
    * @return
    */
  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

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
  //def run[A](s: ExecutorService)(a: Par[A]): A
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      override def call(): A = a(es).get()
    })

  /**
    * Exercise 7.3
    * This implementation of `map2` does _not_ respect timeouts.
    * It simply passes the `ExecutorService` on to both `Par` values, waits for the results of the Futures `af` and `bf`,
    * applies `f` to them, and wraps them in a `UnitFuture`.
    * In order to respect timeouts, we'd need a new `Future` implementation that records the amount of time spent evaluating `af`,
    * then subtracts that time from the available time allocated for evaluating `bf`.
    */
  def map2Timeout[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = (es: ExecutorService) => {
    val af = a(es)
    val bf = b(es)
    UnitFuture(f(af.get(), bf.get()))
  }

  /**
    * Exercise 7.4
    * The asyncF evaluates the function result of f asynchronously by using the
    * lazyUnit function.
    */
  def asyncF[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  /**
    * Exercise 7.5
    *
    * This implementation forks the recursive step off to a new logical thread,
    * making it effectively tail-recursive. However, we are constructing
    * a right-nested parallel program, and we can get better performance by
    * dividing the list in half, and running both halves in parallel.
    * See `sequenceBalanced` below.
    *
    */
  def sequenceRight[A](ps: List[Par[A]]): Par[List[A]] = ps match {
    case Nil => unit(Nil)
    case h :: t => map2(h, fork(sequenceRight(t)))(_ :: _)
  }

  /**
    * We define `sequenceBalanced` using `IndexedSeq`, which provides an
    * efficient function for splitting the sequence in half.
    * Using map2 combinator function for combining the two results.
    *
    * @param as
    * @tparam A
    * @return
    */
  def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
    if (as.isEmpty) unit(Vector())
    else if (as.length == 1) map(as.head)(a => Vector(a))
    else {
      val (l, r) = as.splitAt(as.length / 2)
      map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
    }
  }

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    map(sequenceBalanced(ps.toIndexedSeq))(_.toList)

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  /**
    * Exercise 7.6
    */
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val pars: List[Par[List[A]]] =
    //Applies the filter function asynchronously by the list map function
      as.map(asyncF((a: A) => if (f(a)) List(a) else List()))
    map(sequence(pars))(_.flatten) // convenience method on `List` for concatenating a list of lists
  }

  /**
    * Exercise 7.7
    */

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es =>
      if (run(es)(cond).get) t(es) // Notice we are blocking on the result of `cond`.
      else f(es)

  /**
    * Exercise 7.11
    *
    * Running the `n` parallel computation, and then using that to select a parallel
    * computation from `choices`.
    */
  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = {
    es => {
      val indx = run(es)(n).get // Full source files, running the `n` asynchronously through the ExecutorService, and blocking on the result of `n`.
      run(es)(choices(indx))
    }
  }

  def choiceNViaChoiceN[A](a: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] =
    choiceN(map(a)(b => if (b) 0 else 1))(List(ifTrue, ifFalse))

  /**
    * Exercise 7.12
    */
  def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] = {
    es => {
      val k = run(es)(key).get
      run(es)(choices(k))
    }
  }

  /**
    * Exercise 7.13
    */
  def chooser[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] = {
    es => {
      val k = run(es)(pa).get //Getting the result k of type `A`
      run(es)(choices(k)) // Calling the function `choices` that takes an A and returns a Par[B] type.
    }
  }

  /**
    * `chooser` is usually called `flatMap` or `bind`.
    */
  def flatMap[A, B](p: Par[A])(choices: A => Par[B]): Par[B] =
    es => {
      val k = run(es)(p).get
      run(es)(choices(k))
    }

  def choiceViaFlatMap[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    flatMap(cond)(a => if (a) t else f)

  def choiceNViaFlatMap[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    flatMap(n)(a => choices(a))

  /**
    * Exercise 7.14
    */
  def join[A](a: Par[Par[A]]): Par[A] = es => {
    run(es)(run(es)(a).get())
  }

  def flatMapViaJoin[A, B](p: Par[A])(f: A => Par[B]): Par[B] =
    join(map(p)(f))

  def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] =
    flatMap(a)(a => a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true

    def get(timeout: Long, units: TimeUnit) = get

    def isCancelled = false

    def cancel(evenIfRunning: Boolean): Boolean = false
  }
}
