package com.fpinscala.ch9.xtra

/**
  * Created by havard on 5/15/17.
  */
import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}

/**
  * @author Michael
  */

object Par {
  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true

    def get(timeout: Long, units: TimeUnit) = get

    def isCancelled = false

    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  def async[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B] = a => async(f(a))

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

  def sequence[A](l: List[Par[A]]): Par[List[A]] = l match {
    case Nil => unit(Nil)
    case h :: Nil => map(h)(List(_))
    case first :: second :: tail =>
      val firstTwo = map2(first, second)((f, s) => f :: s :: Nil)
      map2(firstTwo, sequence(tail))((l1, l2) => l1 ++ l2)
  }

  def parMap[A, B](l: List[A])(f: A => B): Par[List[B]] =
    fork(sequence(l.map(asyncF(f))))

  def parFilter[A](l: List[A])(p: A => Boolean): Par[List[A]] = {
    val lifter: A => Par[(A, Boolean)] = asyncF(a => (a, p(a)))
    val seq = sequence(l.map(lifter))
    map(seq)(_.filter(_._2).map(_._1))
  }

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    es => choices(run(es)(n).get())(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    choiceN(map(cond)(b => if (b) 0 else 1))(List(t, f))

  def chooser[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] =
    es => choices(run(es)(pa).get())(es)

  def choice2[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    chooser(cond)(b => if (b) t else f)

  def choiceN2[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    chooser(n)(i => choices(i))

  def join[A](a: Par[Par[A]]): Par[A] =
    es => map(a)(pa => run(es)(pa).get())(es)

  def flatMap[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] =
    join(map(pa)(a => choices(a)))

  def join2[A](ppa: Par[Par[A]]): Par[A] =
    flatMap(ppa)(pa => pa)

  def equal[A](p1: Par[A], p2: Par[A]): Par[Boolean] =
    Par.map2(p1, p2)(_ == _)

  trait ParallelOps {
    def map2[A, B, C](first: Par[A], second: Par[B])(f: (A, B) => C): Par[C]
  }
}
