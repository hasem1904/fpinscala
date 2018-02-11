package com.fpinscala.ch12

import com.fpinscala.ch10.Monoid
import com.fpinscala.ch11.Functor
import com.fpinscala.ch6.State

import scala.language.{higherKinds, implicitConversions}

/**
  *
  * Xtra Code:
  * https://github.com/facaiy/book_notes/blob/master/Manning_Functional_Programming_in_Scala/src/test/scala/io/github/facaiy/fp/scala/c12/ApplicativeSuite.scala
  *
  * @tparam F Type constructor
  */
trait Applicative[F[_]] extends Functor[F] {
  /** Exercise 12 .1 */
  //Primitive combinators
  //def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = apply(map(fa)(f.curried))(fb)

  // `map2` is implemented by first currying `f` so we get a function
  // of type `A => B => C`. This is a function that takes `A` and returns
  // another function of type `B => C`. So if we map `f.curried` over an
  // `F[A]`, we get `F[B => C]`. Passing that to `apply` along with the
  // `F[B]` will give us the desired `F[C]`.
  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    apply(map(fa)(f.curried))(fb)

  /** Exercise 12.2: Define in terms of map2 and unit */
  //def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] = map2(fab, fa)((ab, a) => ab(a))
  // We simply use `map2` to lift a function into `F` so we can apply it
  // to both `fab` and `fa`. The function being lifted here is `_(_)`,
  // which is the same as the lambda notation `(f, x) => f(x)`. That is,
  // It's a function that takes two arguments:
  //   1. A function `f`
  //   2. An argument `x` to that function
  // and it simply applies `f` to `x`.
  def apply[A,B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab, fa)(_(_))

  def unit[A](a: => A): F[A]

  //Derived combinators
  /** Note: Implement map in terms of map2 and unit. */
  //def map[A, B](fa: F[A])(f: A => B): F[B] = map2(fa, unit(()))((a, _) => f(a))
  //def map[A, B](fa: F[A])(f: A => B): F[B] = apply(unit(f))(fa)
  def map[A,B](fa: F[A])(f: A => B): F[B] =
    apply(unit(f))(fa)

  /**
    * Exercise 12.1
    */
  def sequence[A](fas: List[F[A]]): F[List[A]] =
    traverse(fas)(fa => fa)

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))

  //Using List.fill to create a List containing the results of some element computation a number of times.
  //The List is the transformed into a F[List[A]] by calling sequence.
  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  def factor[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa, fb)((_,_))

  //def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = map2(fa, fb)((_, _))
  /**
    * Exercise 12.8
    */
  def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = {
    val self = this
    new Applicative[({type f[x] = (F[x], G[x])})#f] {
      def unit[A](a: => A): (F[A], G[A]) = (self.unit(a), G.unit(a)) //Unit for product (self, G)
      override def apply[A, B](fs: (F[A => B], G[A => B]))(p: (F[A], G[A])): (F[B], G[B]) =
        (self.apply(fs._1)(p._1), G.apply(fs._2)(p._2))
    }
  }

  /**
    * Exercise 12.9
    */
  def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] = {
    val self = this
    new Applicative[({type f[x] = F[G[x]]})#f] {
      def unit[A](a: => A) = self.unit(G.unit(a))
      override def map2[A, B, C](fga: F[G[A]], fgb: F[G[B]])(f: (A, B) => C): F[G[C]] =
        self.map2(fga, fgb)(G.map2(_, _)(f))
    }
  }

  /**
    * Exercise 12.12
    * Implement sequence over a Map.
    *
    * The standard library lets you treat a `Map` as essentially a list of pairs.
    */
  def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] =
    (ofa foldLeft unit(Map.empty[K,V])) {
      case (acc, (k, fv)) => map2(acc, fv)((m, v) => m + (k -> v)) //Adding key/value pair to map in functor F
    }

  /**
    * Exercise 12.10
    *
    * If `self` and `G` both satisfy the laws, then so does the composite.
    * The full proof of the laws can be found at:
    * https://github.com/runarorama/sannanir/blob/master/Applicative.v
    */

  /**
    * Exercise 12.11
    * You want to try writing `flatMap` in terms of `Monad[F]` and `Monad[G]`.
    *
    * def flatMap[A,B](mna: F[G[A]])(f: A => F[G[B]]): F[G[B]] =
    *  self.flatMap(na => G.flatMap(na)(a => ???))
    *
    * Here all you have is `f`, which returns an `F[G[B]]`.
    * For it to have the appropriate type to return from the argument to `G.flatMap`,
    * you'd need to be able to "swap" the `F` and `G` types.
    * In other words, you'd need a _distributive law_.
    * Such an operation is not part of the `Monad` interface.
    *
    */


  /**
    * Exercise 12.3
    *
    * f:(A,B) => C, f.curried has type: A => B => C
    */
  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =

  /**
    * The pattern is simple. We just curry the function
    * we want to lift, pass the result to `unit`, and then `apply`
    * as many times as there are arguments.
    * Each call to `apply` is a partial application of the function
    */
    apply(apply(apply(unit(f.curried))(fa))(fb))(fc)

  def map4[A, B, C, D, E](fa: F[A],
                          fb: F[B],
                          fc: F[C],
                          fd: F[D]
                         )(f: (A, B, C, D) => E): F[E] =
    apply(apply(apply(apply(unit(f.curried))(fa))(fb))(fc))(fd)
}

/** Companion object. */
object Applicative {

  /** Example code of 'Option applicative' */
  val optionApplicative: Applicative[Option] = new Applicative[Option] {
    override def unit[A](a: => A): Option[A] = Some(a)
    override def map2[A, B, C](fa: Option[A], fb: Option[B])(f: (A, B) => C): Option[C] = (fa, fb, f) match {
      case (Some(a), Some(b), _) => Some(f(a, b))
      case _ => None
    }
  }

  /** Applicative functor, but not Monad */
  val streamApplicative = new Applicative[Stream] {
    def unit[A](a: => A): Stream[A] =
      Stream.continually(a) // The infinite, constant stream
    override def map2[A, B, C](a: Stream[A], b: Stream[B])(f: (A, B) => C): Stream[C] =
      a zip b map f.tupled // Combine elements pointwise

    /**
      * Exercise 12.4
      * What is the meaning of StreamApplicative.sequence?
      * Specializing the signature of sequence to stream, we have this:
      *
      * Answer:
      * This transposes the list! That is, we start with a list of rows, each of which is possibly infinite in length.
      * We get back a single row, where each element is the column of values at that position. Try it yourself in the REPL.
      */
    //override def sequence[A](a : List[Stream[A]]): Stream[List[A]] = ???
  }

  /**
    * Exercise 12.6
    *
    */
  def validationApplicative[E]: Applicative[({type f[x] = Validation[E, x]})#f] =
    new Applicative[({type f[x] = Validation[E, x]})#f] {
      def unit[A](a: => A): Validation[E, A] = Success(a)
      override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] = (fa, fb) match {
        case (Success(a), Success(b)) => Success(f(a, b))
        case (Failure(h1, t1), Failure(h2, t2)) => Failure(h1, t1 ++ Vector(h2) ++ t2) //Storing all failures in
        case (e@Failure(_, _), _) => e //Other kinds of errors
        case (_, e@Failure(_, _)) => e //Other kinds of errors
      }
  }

  /**
    * Exercise 12.7
    * Prove that all monads are applicative functors by showing that if the monad laws holds,
    * the Monad impl. of map2, and map satisfy the applicative laws:
    *
    * Answer:
    * We'll just work through left and right identity, but the basic idea for all these proofs is to substitute the definition of all functions,
    * then use the monad laws to make simplifications to the applicative identities.
    *
    * Let's start with left and right identity:
    *
    * map2(unit(()), fa)((_,a) => a) == fa // Left identity
    * map2(fa, unit(()))((a,_) => a) == fa // Right identity
    *
    * We'll do left identity first. We expand definition of `map2`:
    *
    * def map2[A,B,C](fa: F[A], fb: F[B])(f: (A,B) => C): F[C]
    * flatMap(fa)(a => map(fb)(b => f(a,b)))
    *
    * flatMap(unit())(u => map(fa)(a => a)) == fa
    *
    * We just substituted `unit(())` and `(_,a) => a` in for `f`. `map(fa)(a => a)` is just `fa` by the functor laws, giving us:
    *
    * flatMap(unit())(u => fa) == fa
    *
    * Recall that `flatMap` can be rewritten using `compose`, by using `Unit` as the argument to the first function.
    *
    * compose(unit, u => fa)(()) == fa
    *
    * And by the monad laws:
    *
    * compose(unit, f) == f
    *
    * Therefore, `compose(unit, u => fa)` simplifies to `u => fa`. And `u` is just `Unit` here, and is ignored, so this is equivalent to `fa`:
    *
    * (u => fa)(()) == fa
    * fa == fa
    *
    * Right identity is symmetric; we just end up using the other identity for `compose`, that `compose(f, unit) == f`.
    *
    * flatMap(fa)(a => map(unit(()))(u => a)) == fa
    * flatMap(fa)(a => unit(a)) == fa  // via functor laws
    * compose(u => fa, unit)(()) == fa
    * (u => fa)(()) == fa
    * fa == fa
    *
    * Associativity and naturality are left as an exercise.
    */

  type Const[A, B] = A

  /**
    * Turning a Monoid into an Applicative
    */
  implicit def monoidApplicative[M](M: Monoid[M]) =
    new Applicative[({type f[x] = Const[M, x]})#f] {
      def unit[A](a: => A): M = M.zero
      override def apply[A, B](m1: M)(m2: M): M = M.op(m1, m2)
  }
}

/**
  * A minimal implementation of Monad must implement
  * 'unit' and override either 'flatMap' or 'join' and 'map'.
  */
trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B] =
    join(map(ma)(f))

  override def apply[A, B](mf: F[A => B])(ma: F[A]): F[B] =
    flatMap(mf)(f => map(ma)(f))

  override def map[A,B](m: F[A])(f: A => B): F[B] =
    flatMap(m)(a => unit(f(a)))

  override def map2[A,B,C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)
}

/** Monad companion object */
object Monad {
  /**
    * Exercise 12.5
    * Write a monad instance for the Either type: need to fix this...
    */
  def eitherMonad[E]: Monad[({type f[x] = Either[E, x]})#f] =
    new Monad[({type f[x] = Either[E, x]})#f] {
      def unit[A](a: => A): Either[E, A] = Right(a)
      override def flatMap[A, B](eea: Either[E, A])(f: A => Either[E, B]) = eea match {
        case Right(a) => f(a)
        case Left(b) => Left(b)
      }
  }

  def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    override def flatMap[A, B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st flatMap f
  }

  // Monad composition
  def composeM[G[_],H[_]](implicit G: Monad[G], H: Monad[H], T: Traverse[H]):
  Monad[({type f[x] = G[H[x]]})#f] = new Monad[({type f[x] = G[H[x]]})#f] {
    def unit[A](a: => A): G[H[A]] = G.unit(H.unit(a))
    override def flatMap[A,B](mna: G[H[A]])(f: A => G[H[B]]): G[H[B]] =
      G.flatMap(mna)(na => G.map(T.traverse(na)(f))(H.join))
  }

  //def composeM[F[_], N[_]](implicit F: Monad[F], N: Monad[N], T: Traverse[N]): Monad[({type f[x] = F[N[x]]})#f] = ???
}

  /** Applicative functor, but not Monad */
sealed trait Validation[+E, +A]
case class Failure[E](head: E, tail: Vector[E] = Vector()) extends Validation[E, Nothing]
case class Success[A](a: A) extends Validation[Nothing, A]

// The `get` and `set` functions on `State` are used above,
// but aren't in the `exercises` subproject, so we include
// them here
object StateUtil {
  def get[S]: State[S, S] = State(s => (s, s))
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}
















