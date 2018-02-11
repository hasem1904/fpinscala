package com.fpinscala.ch12

import com.fpinscala.ch10.Monoid
import com.fpinscala.ch10.Monoid.Foldable
import com.fpinscala.ch11.Functor
import com.fpinscala.ch6.State

trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
  self =>

  import com.fpinscala.ch6.State.{get, set}

  def traverse[M[_] : Applicative, A, B](fa: F[A])(f: A => M[B]): M[F[B]] = sequence(map(fa)(f))

  def sequence[M[_] : Applicative, A](fma: F[M[A]]): M[F[A]] = traverse(fma)(ma => ma)

  type Id[A] = A

  val idMonad = new Monad[Id] {
    def unit[A](a: => A) = a

    override def flatMap[A, B](a: A)(f: A => B): B = f(a)
  }

  /**
    * Exercise 12.14
    *
    * This implementation is suggestive of laws for `traverse`, since we expect this implementation to obey the usual functor laws.
    * See the chapter notes for discussion of the laws for `Traverse`.
    * Note that we can define `traverse` in terms of `sequence` and `map`,
    * which means that a valid `Traverse` instance may define `sequence` and `map`, or just `traverse`:
    */
  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    traverse[Option, A, B](fa)(a => Some(f(a)))(Applicative.optionApplicative).get

  //def map[A,B](fa: F[A])(f: A => B): F[B] = traverse[Id, A, B](fa)(f)(idMonad)

  override def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    traverse[({type f[x] = Applicative.Const[B, x]})#f, A, Nothing](as)(f)(Applicative.monoidApplicative(mb))

  def traverseS[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({type f[x] = State[S, x]})#f, A, B](fa)(f)(Monad.stateMonad)

  def zipWithIndex_[A](ta: F[A]): F[(A, Int)] =
    traverseS(ta)((a: A) => for {
      i <- get[Int]
      _ <- set(i + 1)
    } yield (a, i)).run(0)._1

  def toList_[A](fa: F[A]): List[A] =
    traverseS(fa)((a: A) => for {
      as <- get[List[A]] // Get the current state, the accumulated list.
      _ <- set(a :: as) // Add the current element and set the new list as the new state.
    } yield ()).run(Nil)._2.reverse

  def mapAccum[S, A, B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa)((a: A) => for {
      s1 <- get[S]
      (b, s2) = f(a, s1)
      _ <- set(s2)
    } yield b).run(s)

  override def toList[A](fa: F[A]): List[A] =
    mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2.reverse

  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
    mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1

  /**
    * Exercise 12.15
    * It's because `foldRight`, `foldLeft`, and `foldMap` don't give us any way of constructing a value of the foldable type.
    * In order to `map` over a structure, you need the ability to create a new structure (such as `Nil` and `Cons` in the case of a `List`).
    *
    * `Traverse` is able to extend `Functor` precisely because a traversal preserves the original structure.
    * An example of a Foldable that is not a functor:
    * *
    * case class Iteration[A](a: A, f: A => A, n: Int) {
    * def foldMap[B](g: A => B)(M: Monoid[B]): B = {
    * def iterate(n: Int, b: B, c: A): B =
    * if (n <= 0) b else iterate(n-1, g(c), f(a))
    * iterate(n, M.zero, a)
    * }
    * }
    * *
    * This class conceptually represents a sequence of `A` values, generated by repeated function application starting from some seed value.
    * But can you see why it's not possible to define `map` for this type?
    */

  /**
    * Exercise 12.16
    */
  def reverse[A](fa: F[A]): F[A] =
    mapAccum(fa, toList(fa).reverse)((_, as) => (as.head, as.tail))._1

  /**
    * Exercise 12.17
    * Use mapAccum to give a default implementation of foldLeft for the Traverse trait
    */
  override def foldLeft[A, B](fa: F[A])(z: B)(f: (B, A) => B): B =
    mapAccum(fa, z)((a, b) => ((), f(b, a)))._2

  def zip[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    mapAccum(fa, toList(fb)) {
      case (a, Nil) => sys.error("zip: Incompatible shapes.")
      case (a, b :: bs) => ((a, b), bs)
    }._1

  def zipL[A, B](fa: F[A], fb: F[B]): F[(A, Option[B])] =
    mapAccum(fa, toList(fb)) {
      case (a, Nil) => ((a, None), Nil)
      case (a, b :: bs) => ((a, Some(b)), bs)
    }._1

  def zipR[A, B](fa: F[A], fb: F[B]): F[(Option[A], B)] =
    mapAccum(fb, toList(fa)) {
      case (b, Nil) => ((None, b), Nil)
      case (b, a :: as) => ((Some(a), b), as)
    }._1


  /**
    * Exercise 12.18
    *
    * Given two functions f and g, traverse fa a singe time, collecting the result of both
    * functions at once.
    */
  def fuse[G[_], H[_], A,B](fa: F[A])(f:A => G[B], g:A => H[B])(implicit G: Applicative[G], H: Applicative[H]):(G[F[B]], H[F[B]]) = {
    traverse[({type f[x] = (G[x], H[x])})#f, A, B](fa)(a =>(f(a), g(a)))(G product H)
  }

  /**
    * Exercise 12.19
    * Implement the composition of two traverse instances.
    */
  def compose[G[_]](implicit G: Traverse[G]): Traverse[({type f[x] = F[G[x]]})#f] =
    new Traverse[({type f[x] = F[G[x]]})#f] {
      override def traverse[M[_] : Applicative, A, B](fa: F[G[A]])(f: A => M[B]): M[F[G[B]]] = {
        self.traverse(fa)((ga: G[A]) => G.traverse(ga)(f))
      }
    }

  /**
    * Exercise 12.20
    * Implement the composition of two monads where one of them is traversable.
    */
  def composeM[G[_], H[_]](implicit G: Monad[G], H:Monad[H], T: Traverse[H]): Monad[({type f[x] = G[H[x]]})#f] =
    new Monad[({type f[x] = G[H[x]]})#f] {
      override def unit[A](a: => A): G[H[A]] = G.unit(H.unit(a))
      override def flatMap[A, B](ma: G[H[A]])(f: A => G[H[B]]): G[H[B]] =
        G.flatMap(ma)(a =>G.map(T.traverse(a)(f))(H.join))
    }
}

case class Tree[+A](head: A, tail: List[Tree[A]])

object Traverse {

  /** Exercise 12.13
    * Write Traverse instances of List, Option and Tree.
    */
  val listTraverse = new Traverse[List] {
    /** * Exercise 12.13 */
    override def traverse[M[_], A, B](as: List[A])(f: A => M[B])(implicit M: Applicative[M]): M[List[B]] =
      as.foldRight(M.unit(List[B]()))((a, fbs) => M.map2(f(a), fbs)((a, b) => a :: b)) // return a functor M of list with type B
  }

  val optionTraverse = new Traverse[Option] {
    /** * Exercise 12.13 */
    override def traverse[G[_], A, B](oa: Option[A])(f: A => G[B])(implicit G: Applicative[G]): G[Option[B]] =
      oa match {
        //case Some(a) => G.map(f(a))(Some(_))
        case Some(a) => G.map(f(a))(a => Some(a))
        case None => G.unit(None)
      }
  }

  val treeTraverse = new Traverse[Tree] {
    /** * Exercise 12.13 */
    override def traverse[G[_], A, B](ta: Tree[A])(f: A => G[B])(implicit G: Applicative[G]): G[Tree[B]] =
      G.map2(f(ta.head), listTraverse.traverse(ta.tail)(a => traverse(a)(f)))((h, t) => Tree(h, t))
  }

  // An example of a Foldable that is not a functor
  case class Iteration[A](a: A, f: A => A, n: Int) {
    def foldMap[B](g: A => B)(M: Monoid[B]): B = {
      def iterate(n: Int, b: B, c: A): B =
        if (n <= 0) b else iterate(n - 1, g(c), f(a))

      iterate(n, M.zero, a)
    }
  }

}