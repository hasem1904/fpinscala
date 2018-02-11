package com.fpinscala.ch11

import com.fpinscala.ch6.State
import com.fpinscala.ch7.Par.Par
import com.fpinscala.ch9.xtra.{Gen, Par}

/**
  * A functor trait that is parametric with the type constructor F[_]
  *
  * @tparam F Type constructor
  */
trait Functor[F[_]] {
  /**
    * A data type that implements the map function.
    *
    * @param fa the concrete data type like e.g. Gen, Parser or Option.
    * @param f  function that takes an input parameter of type A and returns a type B.
    * @tparam A type.
    * @tparam B type.
    * @return A F[B] type constructor.
    */
  def map[A, B](fa: F[A])(f: A => B): F[B]

  /**
    * Distribute the Functor F over the pair to get (F[A], F[B])
    * The distribute combinator, method acts like a generic unzip function.
    * Can reuse the distribute combinator for any type that allows an implementation of map.
    *
    * @param fab
    * @tparam A
    * @tparam B
    * @return
    */
  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  /**
    * The codistribute combinator, may be used as a generator for A or a generator for B.
    * We can construct a generator that produces either A or B depending on which generator we actually have.
    * Can reuse the distribute combinator for any type that allows an implementation of map.
    *
    * Opposite operation over a sum or product.
    *
    * @param e
    * @tparam A
    * @tparam B
    * @return
    */
  def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(fa) => map(fa)(Left(_))
    case Right(fb) => map(fb)(Right(_))
  }
}


trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]

  //def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]

  //= join(map(ma)(f))

  def map[A, B](ma: F[A])(f: A => B): F[B] =
    flatMap(ma)(a => unit(f(a)))

  def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  /**
    * Exercise 11.3
    * Implement sequence and traverse combinators in the Monad[F]:
    *
    * Hint: These implementations should be very similar to implementations from previous chapters,
    * only with more general types, and using the functions on the `Monad` trait. Make use of `unit` and `map2`.
    */

  /** Repeats the 'ma' monadic value 'n' times and gather the result in a single value
    * where the monad `M` determines how values are actually combined. */
  def sequence[A](lma: List[F[A]]): F[List[A]] =
    lma.foldRight(unit(List[A]()))((ma, mla) => map2(ma, mla)(_ :: _))

  def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] =
    la.foldRight(unit(List[B]()))((a, mlb) => map2(f(a), mlb)(_ :: _))

  /**
    * Exercise 11.4
    * Implement replicateM
    */
  // Using `sequence` and the `List.fill` function of the standard library:
  def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
    sequence(List.fill(n)(ma))

  // For `List`, the `replicateM` function will generate a list of lists.
  // It will contain all the lists of length `n` with elements selected from the
  // input list.
  // For `Option`, it will generate either `Some` or `None` based on whether the
  // input is `Some` or `None`. The `Some` case will contain a list of length `n`
  // that repeats the element in the input `Option`.
  // The general meaning of `replicateM` is described very well by the
  // implementation `sequence(List.fill(n)(ma))`. It repeats the `ma` monadic value
  // `n` times and gathers the results in a single value, where the monad `M`
  // determines how values are actually combined.

  // Recursive version:
  def _replicateM[A](n: Int, ma: F[A]): F[List[A]] =
    if (n <= 0) unit(List[A]()) else map2(ma, _replicateM(n - 1, ma))(_ :: _)


  /**
    * Exercise 11.5
    * Think about how replicateM behave for various choices of F.
    *
    * For `List`, the `replicateM` function will generate a list of lists.
    * It will contain all the lists of length `n` with elements selected from the
    * input list.
    * For `Option`, it will generate either `Some` or `None` based on whether the
    * input is `Some` or `None`. The `Some` case will contain a list of length `n`
    * that repeats the element in the input `Option`.
    *
    * THE GENERAL MEANING OF `replicateM` is described very well by the
    * implementation `sequence(List.fill(n)(ma))`:
    * It repeats the `ma` monadic value `n` times and gathers the results in a single value, where the monad `M`
    * determines how values are actually combined.
    */

  /** product combinator */
  def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))

  /**
    * Exercise 11.6
    * Implement the filterM function
    */
  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
    ms.foldRight(unit(List[A]()))((x, y) =>
      compose(f, (b: Boolean) => if (b) map2(unit(x), y)(_ :: _) else y)(x))

  /**
    * Exercise 11.7
    * Implement the compose method.
    * f = Functions of types like: (f:A => F[B])
    * g = FUnctions of types like: (g:B => F[C])
    *
    * compose(compose(f,g), h) == compose(f, compose(g, h))
    */
  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  /**
    * Exercise 11.8
    * Implement flatMap in terms of compose
    */
  /*def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B] =
    compose((_: Unit) => ma, f)(())*/

  /**
    * Exercise 11.9
    * Show that the two formulations of associative law, the one in terms of flatMap and the on in terms of
    * of compose are equivalent:
    *
    * Let's rewrite the following in terms of `flatMap`:
    *
    * compose(compose(f, g), h) == compose(f, compose(g, h))
    *
    * a => flatMap(compose(f, g)(a))(h) == a => flatMap(f(a))(compose(g, h))
    * a => flatMap((b => flatMap(f(b))(g))(a))(h) == a => flatMap(f(a))(b => flatMap(g(b))(h))
    *
    * So far we have just expanded the definition of `compose`. Equals substituted for equals.
    * Let's simplify the left side a little:
    *
    * a => flatMap(flatMap(f(a))(g))(h) == a => flatMap(f(a))(b => flatMap(g(b))(h))
    *
    * Let's simplify again by eliminating the `a` argument and substituting a hypothetical value `x` for `f(a)`:
    *
    * flatMap(flatMap(x)(g))(h) == flatMap(x)(b => flatMap(g(b))(h))
    *
    * This now looks exactly like the monad law stated in terms of `flatMap`, just with different names:
    *
    * flatMap(flatMap(x)(f))(g) == flatMap(x)(a => flatMap(f(a))(g))
    *
    */

  /**
    * Exercise 11.10
    * Prove that these two statements of the identity laws are equivalent
    *
    * We simply substitute the definition of `compose` in terms of `flatMap`.
    *
    * compose(f, unit)(v) == f(v)           // for all functions f and values v
    *
    * (a => flatMap(f(a))(unit))(v) == f(v) // Expand `compose`
    *
    * flatMap(f(v))(unit) == f(v)           // Simplify function application    *
    * flatMap(x)(unit) == x                 // Abstract out `f(v)`
    *
    * compose(unit, f)(x) == f(x)
    * flatMap(unit(x))(f) == f(x) // Expand `compose`
    *
    */

  /**
    * Exercise 11.11
    * Prove that identity laws hold for monad of your choice.
    *
    * For `Option`, we again consider both cases `None` and `Some` and expand the equation.
    * The monadic `unit` is the `Some(_)` constructor.
    *
    * Left identity is trivially true for None:
    * flatMap(None)(Some(_)) == None
    *
    * And here it is for Some:
    * flatMap(Some(v))(Some(_)) == Some(v)
    * Substitute the definition of `flatMap`:
    * Some(v) == Some(v)
    *
    * Right identity is just as easy for None:
    * flatMap(Some(None))(f) == f(None)
    * Substitute definition of flatMap:
    * f(None) == f(None)
    *
    * And for Some:
    * flatMap(Some(Some(v)))(f) == f(Some(v))
    * Substitute definition of flatMap:
    * f(Some(v)) == f(Some(v))
    */

  /**
    * Exercise 11.12
    * There's a third minimal set of monadic combinators: map, unit and join.
    * Implement join in terms of flatMap.
    *
    * Note:
    * Follow the types here. Remember that `A` can be _any type at all_, including the type `F[B]` for some type `B`.
    *
    * Here:
    * mma: F[F[A] == F[A]
    * ma: A => F[B]
    *
    * flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]
    */
  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)

  /**
    * Exercise 11.13
    * Implement either flatMap or compose in terms of "join and map":
    *
    * Note:
    * Join is sometimes called "flatten", and `flatMap` "maps and then flattens".
    */
  def flatMap[A, B](ma: F[A])(f: A => F[B]) = join(map(ma)(f))

  /*def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    (a: A) => join(map(f(a))(g))*/

  /**
    * Exercise 11.14
    *
    * Rewrite the monad laws stated in terms of `flatMap` by substituting your implementation of `join`.
    *
    *
    * We can look at the associative law in terms of `flatMap` from another perspective.
    * It says that `x.flatMap(f).flatMap(g)` is equal to `x.flatMap(a => f(a).flatMap(g))` _for all_ choices of `f` and `g`.
    * So let's pick a particular `f` and `g` that's easy to think about. We can just pick the identity function:
    *
    * x.flatMap(z => z).flatMap(z => z) == x.flatMap(a => a.flatMap(z => z))
    *
    * And of course, flatMapping with the identify function is `join`!
    * The associative law can now be stated as:
    *
    * join(join(x)) == x.flatMap(join)
    *
    * And we know that `flatMap` is "map, then join," so let's eliminate this last call to `flatMap`:
    *
    * join(join(x)) == join(map(x)(join))
    *
    * The identity laws in terms of `join` are:
    *
    * join(map(x)(unit)) == x
    * join(unit(x)) == x
    *
    * This follows from the definition of `join` and the identity laws in terms of `flatMap`:
    *
    * flatMap(x)(unit) == x
    * flatMap(unit(x))(f) == f(x)
    *
    * For the second law, we simply substitute the identity function for `f`, which gives us `join`.
    *
    * Let's make a fast-and-loose proof for this version of the associative law using the `List` monad as an example.
    * Of course, `join` in the `List` monad is just _list concatenation_:
    *
    * scala> listMonad.join(List(List(1, 2), List(3, 4)))
    * res0: List[Int] = List(1, 2, 3, 4)
    *
    * Now let's say we have some lists, nested to a depth of three:
    *
    * val ns: List[List[List[Int]]] =
    * List(List(List(1,2), List(3,4)),
    * List(List(), List(5)))
    *
    * If we `join` this list, the outer lists get concatenated and we have a list of lists two levels deep:
    *
    * scala> ns.flatten
    * res1: List[List[Int]] = List(List(1, 2), List(3, 4), List(), List(5))
    *
    * If we instead _map_ `join` over it, we get a different nesting but again two levels deep. This flattens the _inner_ lists.
    *
    * scala> ns.map(listMonad.join)
    * res2: List[List[Int]] = List(List(1, 2, 3, 4), List(5))
    *
    * And then joining `res1` should be the same as joining `res2`:
    *
    * scala> listMonad.join(res1) == listMonad.join(res2)
    * res3: Boolean = true
    *
    * So all that the associative law is saying for the `List` monad is that concatenating the outer lists and then the inner ones (`join(join(ns))`)
    * is the same as first concatenating the inner lists and then the outer ones (`join(ns.map(join))`).
    *
    */

  /**
    * Exercise 11.15
    * Associative law means for Par and Parser:
    *
    * We can state the associative law in terms of `join`:
    *
    * join(join(x)) == join(map(x)(join))
    *
    * For `Par`, the `join` combinator means something like "make the outer thread wait for the inner one to finish."
    * What this law is saying is that if you have threads starting threads three levels deep, then joining the inner
    * threads and then the outer ones is the same as joining the outer threads and then the inner ones.
    *
    * For `Parser`, the `join` combinator is running the outer parser to produce a `Parser`,
    * then running the inner `Parser` _on the remaining input_. The associative law is saying,
    * roughly, that only the _order_ of nesting matters, since that's what affects the order in which the parsers are run.
    */

  /**
    * Exercise 11.16
    * Identity laws are stating in concrete terms for Gen and List.
    *
    * Recall the identity laws:
    *
    * left identity:  flatMap(unit(x))(f) == f(x)
    * right identity: flatMap(x)(unit)    == x
    *
    * The left identity law for `Gen`:
    * The law states that if you take the values generated by `unit(x)` (which are always `x`) and apply `f` to those values,
    * that's exactly the same as the generator returned by `f(x)`.
    *
    * The right identity law for `Gen`:
    * The law states that if you apply `unit` to the values inside the generator `x`, that does not in any way differ from `x` itself.
    *
    * The left identity law for `List`:
    * The law says that wrapping a list in a singleton `List` and then flattening the result is the same as doing nothing.
    *
    * The right identity law for `List`:
    * The law says that if you take every value in a list, wrap each one in a singleton `List`, and then flatten the result, you get the list you started with.
    */
}


object Monad /*extends App*/ {
  val listFunctor = new Functor[List] {
    /**
      * A data type that implements the map function.
      * A type constructor like List is a functor, and the Functor[F] instance constitutes proof that F is in fact a
      * functor.
      *
      * @param as the concrete data type like e.g. Gen, Parser or Option.
      * @param f  function that takes an input parameter of type A and returns a type B.
      * @tparam A type.
      * @tparam B type.
      * @return A F[B] type constructor.
      */
    override def map[A, B](as: List[A])(f: A => B) = as map f
  }

  val genMonad = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)

    override def flatMap[A, B](ma: Gen[A])(f: A => Gen[B]): Gen[B] =
      ma.flatMap(f)
  }

  /** Exercise 11.1
    * Write monad instances for Par, Parser, Option, Stream and List
    *
    * NB! Here: A Monad must have the following primitives/combinators
    * (Monad is a Functor, but a Functor is not a monad):
    *  - unit
    *  - map
    *  - flatMap
    *  - map2
    * */
  val parMonad = new Monad[Par] {
    def unit[A](a: => A): Par[A] = Par.unit(a)

    override def flatMap[A, B](ma: Par[A])(f: A => Par[B]): Par[B] = Par.flatMap(ma)(f)
  }

  /*val parserMonad = new Monad[Parser]{
    def unit[A](a: => A): Parser[A] = ???
    def flatMap[A, B](ma: Parser[A])(f: A => Parser[B]): Parser[B] = ???
  }*/

  val optionMonad = new Monad[Option] {
    def unit[A](a: => A): Option[A] = Some(a)

    override def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] = ma.flatMap(f)
  }

  val streamMonad = new Monad[Stream] {
    def unit[A](a: => A): Stream[A] = Stream(a)

    override def flatMap[A, B](ma: Stream[A])(f: A => Stream[B]): Stream[B] = ma.flatMap(f)
  }

  val listMonad = new Monad[List] {
    def unit[A](a: => A): List[A] = List(a)

    override def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] = ma.flatMap(f)
  }

  /**
    * Exercise 11.2
    * Implement a State Monad
    */
  // Since `State` is a binary type constructor, we need to partially apply it
  // with the `S` type argument. Thus, it is not just one monad, but an entire
  // family of monads, one for each type `S`. One solution is to create a class
  // `StateMonads` that accepts the `S` type argument and then has a _type member_
  // for the fully applied `State[S, A]` type inside:
  class StateMonads[S] {
    type StateS[A] = State[S, A] //`StateS` type constructor

    // We can then declare the monad for the `StateS` type constructor:
    val monad = new Monad[StateS] {
      def unit[A](a: => A): State[S, A] = State(s => (a, s))

      override def flatMap[A, B](st: State[S, A])(f: A => State[S, B]): State[S, B] =  st flatMap f
    }
  }

  // But we don't have to create a full class like `StateMonads`. We can create
  // an anonymous class inline, inside parentheses, and project out its type member,
  // `lambda`:
  def stateMonad[S] = new Monad[({type lambda[x] = State[S, x]})#lambda] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    override def flatMap[A, B](st: State[S, A])(f: A => State[S, B]): State[S, B] = st flatMap f
  }

  /**
    * Exercise 11.18
    *
    * `replicateM` for `State` repeats the same state transition a number of times and returns a list of the results.
    * It's not passing the same starting state many times, but chaining the calls together so that the output state of one is the input state of the next.
    *
    * `map2` works similarly in that it takes two state transitions and feeds the output state of one to the input of the other.
    * The outputs are not put in a list, but combined with a function `f`.
    *
    * `sequence` takes an entire list of state transitions and does the same kind of thing as `replicateM`:
    * it feeds the output state of the first state transition to the input state of the next, and so on. The results are accumulated in a list.
    */

  /**
    * Exercise 11.19
    * Associative and identity laws of the Monads.
    *
    * Getting and setting the same state does nothing:
    * getState.flatMap(setState) == unit(())
    *
    * written as
    * for -comprehension:
    *
    * for
    * {
    * x <- getState
    * _ <- setState(x)
    * } yield ()
    * *
    * Setting the state to `s` and getting it back out yields `s`.
    * setState (s).flatMap(_ => getState) == unit(s)
    * *
    * alternatively:
    *
    * for
    * {
    * _ <- setState(s)
    * x <- getState
    * } yield x
    */

  def stateMonad2[S] = new Monad[({type f[x] = State[S, x]})#f] {
    override def unit[A](a: => A): State[S, A] = State.unit(a)
    override def flatMap[A, B](fa: State[S, A])(f: (A) => State[S, B]): State[S, B] = fa flatMap f
  }
}

/**
  * Exercise 11.20
  * This monad is very similar to the `State` monad, except that it's "read-only".
  * You can "get" but not "set" the `R` value that `flatMap` carries along.
  *
  *
  * The action of Reader's `flatMap` is to pass the `r` argument along to both the
  * outer Reader and also to the result of `f`, the inner Reader. Similar to how
  * `State` passes along a state, except that in `Reader` the "state" is read-only.
  *
  * The meaning of `sequence` here is that if you have a list of functions, you can
  * turn it into a function that takes one argument and passes it to all the functions
  * in the list, returning a list of the results.
  *
  * The meaning of `join` is simply to pass the same value as both arguments to a
  * binary function.
  *
  * The meaning of `replicateM` is to apply the same function a number of times to
  * the same argument, returning a list of the results. Note that if this function
  * is _pure_, (which it should be), this can be exploited by only applying the
  * function once and replicating the result instead of calling the function many times.
  * This means the Reader monad can override replicateM to provide a very efficient
  * implementation.
  *
  */
case class Reader[R, A](run: R => A)

object Reader {
  def readerMonad[R] = new Monad[({type f[x] = Reader[R, x]})#f] {
    def unit[A](a: => A): Reader[R, A] = Reader(_ => a)

    override def flatMap[A, B](st: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] =
      Reader(r => f(st.run(r)).run(r))
  }

  // A primitive operation for it would be simply to ask for the `R` argument:
  def ask[R]: Reader[R, R] = Reader(r => r)
}

/**
  * Exercise 11.17
  * Identity Monad
  */
case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = Id(f(value)) //Create type of Id with the return value of applying the function 'f' with input param 'value' i.o to return correct type.
  def flatMap[B](f: A => Id[B]): Id[B] = f(value) //Apply function 'f' with input param 'value' i.o to return correct type.
}

/**
  * The Identity Monad, Monad[Id]
  * This is an implementation of the Monad[Id] type.
  *
  * Note: A Monad must implement ONE of the following Monad combinator set:
  * 1. unit and flatMap
  * 2. unit and compose
  * 3. unit, map, join
  *
  * The Monad must also follow the associative and identity law.
  *
  */
object Id {
  val idMonad = new Monad[Id] {
    //Monad must implement one of the "Monad combinator set" and follow the associative and identity law.
    def unit[A](a: => A) = Id(a)

    override def flatMap[A, B](ma: Id[A])(f: A => Id[B]): Id[B] = ma flatMap (f)
  }
}

























