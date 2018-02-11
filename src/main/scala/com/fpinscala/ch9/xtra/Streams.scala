package com.fpinscala.ch9.xtra

/**
  * Created by havard on 5/15/17.
  */
/**
  *
  * @author mle
  */
object Streams {

  sealed abstract class Stream[+A] {
    def uncons: Option[Cons[A]]

    def isEmpty: Boolean = uncons.isEmpty

    def toList: List[A] = uncons map (str => str.head :: str.tail.toList) getOrElse Nil

    def take(n: Int): Stream[A] =
      if (n == 0) Empty
      else uncons map (str => Stream.cons(str.head, str.tail.take(n - 1))) getOrElse Empty

    def takeWhile(p: A => Boolean): Stream[A] =
      uncons map (str => if (p(str.head)) Stream.cons(str.head, str.tail.takeWhile(p)) else Empty) getOrElse Empty

    def foldRight[B](z: => B)(f: (A, => B) => B): B // =
    //      uncons match {
    //        case Some(c) => f(c.head, c.tail.foldRight(z)(f))
    //        case None => z
    //      }

    def exists(p: A => Boolean): Boolean =
      foldRight(false)((a, b) => p(a) || b)

    def find(p: A => Boolean): Option[A] =
      uncons.map(ca => if (p(ca.head)) Some(ca.head) else ca.tail.find(p)) getOrElse None

    /**
      * Stream(1, 2, 3).forall(a => a < 4)
      * Stream(1, 2, 3).foldRight(true)((a, b) => a < 4 && b)
      * 1 < 4 && Stream(2, 3).foldRight(true)((a, b) => a < 4 && b)
      * 1 < 4 && (2 < 4 && Stream(3).foldRight(true)((a, b) => a < 4 && b))
      * 1 < 4 && (2 < 4 && (3 < 4 && Empty.foldRight(true)((a, b) => a < 4 && b)))
      * 1 < 4 && (2 < 4 && (3 < 4 && (true)))
      * true && true && true && true
      * true
      *
      * b is the foldRight on the tail
      */
    def forAll(p: A => Boolean): Boolean =
      foldRight(true)((a, b) => p(a) && b)

    def takeWhileUsingFoldRight(p: A => Boolean): Stream[A] =
      foldRight[Stream[A]](Empty)((a, b) => if (p(a)) Stream.cons(a, b) else Empty)

    def unconsUsingFoldRight: Option[Cons[A]] =
      foldRight[Option[Cons[A]]](None)((a, b) => Some(Stream.cons2(a, b.getOrElse(Empty))))

    def map[B](f: A => B): Stream[B] =
      foldRight[Stream[B]](Empty)((a, b) => Stream.cons(f(a), b))

    def filter(p: A => Boolean): Stream[A] =
      foldRight[Stream[A]](Empty)((a, b) => if (p(a)) Stream.cons(a, b) else b)

    def appendSingle[B >: A](elem: B): Stream[B] =
      foldRight[Stream[B]](Stream.cons(elem, Empty))((a, b) => Stream.cons(a, b))

    def append[B >: A](s: Stream[B]): Stream[B] =
      foldRight[Stream[B]](s)((a, b) => Stream.cons(a, b))

    def flatMap[B](f: A => Stream[B]): Stream[B] =
      foldRight[Stream[B]](Empty)((a, b) => f(a) append b)

    def tails: Stream[Stream[A]] =
      Stream.unfold[Stream[A], Option[Stream[A]]](Some(this))(str => str.map(st => (st, st.uncons.map(_.tail))))

    def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
      tails.map(_.foldRight(z)(f))
  }

  object Empty extends Stream[Nothing] {
    val uncons = None

    def foldRight[B](z: => B)(f: (Nothing, => B) => B): B = z
  }

  sealed abstract class Cons[+A] extends Stream[A] {
    def head: A

    def tail: Stream[A]

    val uncons = Some(this)

    def foldRight[B](z: => B)(f: (A, => B) => B): B =
      f(head, tail.foldRight(z)(f))
  }

  object Stream {
    def empty[A]: Stream[A] = Empty

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = cons2(hd, tl)

    def cons2[A](hd: => A, tl: => Stream[A]): Cons[A] = new Cons[A] {
      lazy val head = hd
      lazy val tail = tl
    }

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) Empty else cons(as.head, apply(as.tail: _*))

    val ones: Stream[Int] = cons(1, ones)

    def constant[A](a: A): Stream[A] = cons(a, constant(a))

    def from(n: Int): Stream[Int] = cons(n, from(n + 1))

    def fibs: Stream[Int] = {
      def fibsInner(prevPrev: Int, prev: Int): Stream[Int] =
        cons(prevPrev, fibsInner(prev, prev + prevPrev))
      fibsInner(0, 1)
    }

    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
      f(z).map(p => cons(p._1, unfold(p._2)(f))).getOrElse(Empty)

    def infiniteUnfold[A, S](z: S)(f: S => (A, S)): Stream[A] = {
      val (a, s) = f(z)
      cons(a, infiniteUnfold(s)(f))
    }

    val onesUsingUnfold: Stream[Int] = unfold(1)(i => Some(1, 1))

    def constantUsingUnfold[A](a: A): Stream[A] =
      unfold(a)(_ => Some((a, a)))

    def fromUsingUnfold(n: Int): Stream[Int] =
      unfold(n)(i => Some((i, i + 1)))

    def fibsUsingUnfold: Stream[Int] =
      unfold((0, 1))(pair => Some((pair._1, (pair._2, pair._1 + pair._2))))

    def map[A, B](s: Stream[A])(f: A => B): Stream[B] =
      unfold(s)(str => str.uncons.map(st => (f(st.head), st.tail)))

    def take[A](s: Stream[A], n: Int): Stream[A] =
      unfold((s, n))(str =>
        if (str._2 > 0) str._1.uncons.map(st => (st.head, (st.tail, str._2 - 1)))
        else None)

    def takeWhile[A](s: Stream[A], p: A => Boolean) = unfold(s)(str =>
      str.uncons.filter(st => p(st.head)).map(st => (st.head, st.tail)))

    def zip[A, B](s1: Stream[A], s2: Stream[B]): Stream[(A, B)] =
      unfold((s1, s2))(streams => {
        streams._1.uncons.flatMap(elem1 => streams._2.uncons.map(elem2 => {
          ((elem1.head, elem2.head), (elem1.tail, elem2.tail))
        }))
      })

    def zipAll[A, B](s1: Stream[A], s2: Stream[B]): Stream[(Option[A], Option[B])] =
      unfold((s1, s2))(streams => {
        val first: Option[(A, Stream[A])] = streams._1.uncons.map(str => (str.head, str.tail))
        val second = streams._2.uncons.map(str => (str.head, str.tail))
        if (first.isEmpty && second.isEmpty) None
        else Some(((first.map(_._1), second.map(_._1)), (first.map(_._2).getOrElse(Empty), second.map(_._2).getOrElse(Empty))))
      })

    def startsWith[A](s1: Stream[A], s2: Stream[A]): Boolean =
      zipAll(s1, s2)
        .takeWhile(pair => pair._2.isDefined)
        .forAll(pair => pair._1.isDefined && pair._1.get == pair._2.get)

    //    def tails[A](s: Stream[A]): Stream[Stream[A]] =
    //      unfold[Stream[A], Option[Stream[A]]](Some(s))(str => str.map(st => (st, st.uncons.map(_.tail))))

    def hasSubsequence[A](s1: Stream[A], s2: Stream[A]): Boolean =
      s1.tails exists (startsWith(_, s2))
  }

}
