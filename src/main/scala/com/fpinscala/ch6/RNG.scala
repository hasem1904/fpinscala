package com.fpinscala.ch6

import scala.annotation.tailrec

/**
  * Created by havard on 2/13/17.
  */
trait RNG {
  /**
    * Combinator(State action): 'Randomly generated A'
    *
    * @tparam A type being used
    */
  type Rand[+A] = RNG => (A, RNG)
  //type Rand[A] = State[RNG, A] //Using the more general State type for representation of state action.

  def nextInt: (Int, RNG)

  def randomPair(rng: RNG): (Int, Int)

  def nonNegativeInt(rng: RNG): (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {

  /**
    * State action (transition action):
    * Using the new type.
    */
  val int: Rand[Int] = _.nextInt //OR NOT anonymous function syntax: r => r.nextInt

  /**
    * unit:
    * RNG state transition, which passes the RNG state through without using it,
    * always returning a constant value rather than a random value
    * unit function let us combine Rand actions(state actions) while avoiding  explicitly passing along the RNG state.
    * The unit function acts as a simple RNG state transition, which passes the RNG state through without using it,
    * always returning a constant value rather than a random value.
    *
    * @param a parameter type
    * @tparam A type being used.
    * @return
    */
  def unit[A](a: A): Rand[A] = rng => (a, rng)

  /**
    * Transforming the output of a state action without modifying the state itself.
    * Remember that Rand[A] is the same as RNG => (A, RNG), so this is just a kind of function composition.
    *
    * @param s note that the type Rand[A], is the same as: RNG => (A, RNG), e.g: Rand[Int]= rng => (Int, rng)
    * @param f transformer function
    * @tparam A type
    * @tparam B type (new result of type A. e.g.: A of type Int, i => i + 1 (B is the type i + 1).
    * @return a new Rand[B] type (with a transformed/new value type as a result of 'f(A)' function, e.g. Rand[Int]
    */
  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  /**
    * nonNegativeEven is a function variable.
    * (Testing how a combinator map function can be used!)
    * An example on how map can be used, which reuses 'nonNegativeEven'
    * to generate Int that is greater than or equal to zero divided  by two.    *
    *
    * Demonstrates the usage of the map function.
    * map Signature:
    * map[A, B](s: Rand[A])(f: A => B): Rand[B] here:
    *
    * 1. (s: Rand[A]): The first argument type list is nonNegativeInt that matches the signature: RNG => (Int, RNG)
    * 2. (f: A => B):  The second argument type list is the anonymous function (i => i - i % 2) that matches the signature: (f: A => B)
    *
    */
  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - 1 % 2)

  def nonNegativeEvenText: Rand[String] = map(nonNegativeInt)(i => "> nonNegativeEvenText: " + (i - 1 % 2).toString)

  /**
    * Exercise 6.6
    */
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, r1) = ra(rng)
      val (b, r2) = rb(r1)
      (f(a, b), r2)
    }


  /**
    * both: combining action.
    * Combines two actions, that e.g. generates type A and B into one action of pairs A and B.
    *
    * @param ra
    * @param rb
    * @tparam A
    * @tparam B
    * @return
    */
  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((a, b) => (a, b)) //Alt.: map2(ra, rb)((_, _))

  /**
    * Using the both state RNG function to combine other arbitrary RNG state actions.
    * Here we combine the RNG state actions 'double' and 'int'.
    **/
  def randIntDouble: Rand[(Int, Double)] = both(int, double)

  //using the int and double functions.
  def randDoubleInt: Rand[(Double, Int)] = both(double, int) //using the double and int functions.

  def nonNegativeLessThan(n: Int): Rand[Int] =
    map(nonNegativeInt) {
      _ % n
    }

  /*def nonNegativeLessThan2(n: Int): Rand[Int] =
    map(nonNegativeInt) { i =>
      val mod = i % n
      if(i + (n-1) - mod >=0) mod else nonNegativeLessThan2(n)(???)
    }*/


  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }

  override def randomPair(rng: RNG): (Int, Int) = {
    //val (i1, _) = rng.nextInt //Generate the same number
    //val (i2, _) = rng.nextInt //Generate the same number
    val (i1, rng2) = rng.nextInt //Generate two distinct numbers, need to use the RNG returned by the first call to 'nextInt'
    val (i2, rng3) = rng2.nextInt //to generate the 'second' int.
    (i1, i2)
  }

  /**
    * Exercise 6.1
    *
    * We need to be quite careful not to skew the generator.
    * Since `Int.Minvalue` is 1 smaller than `-(Int.MaxValue)`,
    * it suffices to increment the negative numbers by 1 and make them positive.
    * This maps Int.MinValue to Int.MaxValue and -1 to 0.
    *
    * @param rng a random number generator type.
    * @return a none negative Int.
    */
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    //Random number between 0 and Int.MaxValue
    (if (i < 0) -(i + 1) else i, r)
  }

  /**
    * Exercise 6.2
    * A function that generates a Double between 0 and 1, not including 1.
    * We generate an integer >= 0 and divide it by one higher than the
    * maximum. This is just one possible solution.
    *
    * @param rng a random number generator type.
    * @return a random double value between 0 and 1 (not inclusive).
    */
  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }

  /**
    * Exercise 6.3
    */
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng1) = rng.nextInt
    val (d, rng2) = double(rng1)
    ((i, d), rng2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), rng1) = intDouble(rng)
    ((d, i), rng1)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d, rng1) = double(rng)
    val (d1, rng2) = double(rng1)
    val (d2, rng3) = double(rng2)
    ((d, d1, d2), rng3)
  }

  /**
    * Exercise 6.4
    */
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def loop(xs: List[Int], n: Int, rng2: RNG): (List[Int], RNG) = {
      if (n <= 0) (xs, rng2)
      else {
        val (i, r) = rng2.nextInt
        loop(i :: xs, n - 1, r)
      }
    }
    loop(List(), count, rng)
  }

  /**
    * Exercise 6.5
    *
    * def double(rng: RNG)
    * map[A, B](s: Rand[A])(f: A => B): Rand[B]
    */
  def doubleViaMap(rng: RNG) =
    map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble + 1))

  /**
    * Exercise 6.6
    *
    * In `sequence`, the base case of the fold is a `unit` action that returns
    * the empty list. At each step in the fold, we accumulate in `acc`
    * and `f` is the current element in the list.
    * `map2(f, acc)(_ :: _)` results in a value of type `Rand[List[A]]`
    * We map over that to prepend (cons) the element onto the accumulated list.
    *
    * We are using `foldRight`. If we used `foldLeft` then the values in the
    * resulting list would appear in reverse order. It would be arguably better
    * to use `foldLeft` followed by `reverse`. What do you think?
    *
    */
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

  def intsViaSequence(count: Int)(rng: RNG): Rand[List[Int]] = {
    sequence(List.fill(count)(int)) //List.fill(count)(int) makes a list with int state function count times.
  }

  /**
    * Exercise 6.8
    * flatMap
    *
    * The first parameter that takes a function 'f' of type 'Rand[A]'
    * Note that Rand[A] is equal to: Rand[A] = RNG => (A, RNG)
    * that is equal to RNG[A] => rng => (a, rng). Here the inner '{}'
    * means that we are handling the first parameter list.
    * (Chaining the result from function 'f' via function 'g' to the result type Rand[B].
    * Note that no return type is set for function f).
    * Then in order to return the right type, Rand[B], we use function 'g' (to return Rand[A]) and pass along the
    * new state of RNG, r1.
    *
    * flatMap allows us to generate a random number A with Rand[A], and then take that A and choose a
    * Rand[B] based on its value.
    */
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (a, r1) = f(rng)
      g(a)(r1) //We pass the new state along.
    }
  }

  def nonNegativeLessThanViaFlatMap(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) {
      i =>
        val mod = i % n
        if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }

  /**
    * Exercise 6.9
    *
    * mapViaFlatMap:
    * Using the unit state action, which passes the RNG state through without using it,
    * always returning a constant value rather than a random value.
    */
  def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a))) //`unit` action that returns a Rand[B] without using it.

  /**
    * Exercise 6.0
    * map2ViaFlatMap
    * (map[A, B](s: Rand[A])(f: A => B): Rand[B])
    *
    **/
  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
  //ra: Rand[A] map(rb)(b => f(a, b): Rand[B]
    flatMap(ra) { a => map(rb)(b => f(a, b)) }

  def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)

}

/**
  * State case class for computation that carries some state along, or state action, state transitions or even statement.
  * Used for capturing common patterns of stateful programs.
  * We can now make Rand a type alias of State.
  *
  * State acts as a more general version of RNG => (A, RNG)
  *
  */

import com.fpinscala.ch6.State._

/**
  * State case class for computation that carries some state along, or state action, state transitions or even statement.
  * Used for capturing common patterns of stateful programs.
  * We can now make Rand a type alias of State.
  *
  * type State[S, +A] = S => (A, S)
  *
  * State acts as a more general version of RNG => (A, RNG)
  *
  * @param run function that takes a type S and result in a tuple two type (A,S), S => (A, S)
  * @tparam S
  * @tparam A
  */
case class State[S, +A](run: S => (A, S)) {
  /**
    * Exercise 6.10
    */

  /**
    * The map combinator function, transforms a (state action) type A to a (state action) type B.
    * Note is uses the unit state action combinator, leaving the state "unchanged" and transforms the type A to type B.
    *
    * @param f function that takes a (state action) type A and transforms it to a (state action) of type B.
    * @tparam B type is the resulting action state type.
    * @return a new state of type (action state) B and a new state S, State[S, B].
    */
  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))

  /**
    * The map2 function combines two action state types A and B into a new state S and state action type C.
    *
    * @param sb state of type State[S, B], note State[S, B] = S => (B, S)
    * @param f  function for combining the state action type A and B into a C
    * @tparam B state action type.
    * @tparam C state action type.
    * @return a new state of type State[S, C].
    */
  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

  /**
    * The flatMap allows us to generate an action 'a' and state 's1' of type A, and take that state
    * and take that A and choose a state State[S, B] based on its value
    * (The new State, State[S, B] is generated based on the state that we get from action of type A and it's state).
    * Note: type State[S, +A] = S => (A, S)
    *
    * @param f function that takes a type A and transforms it to a state of type State[S, B].
    * @tparam B type is the resulting action state type.
    * @return a new state of type (action state) B and a new state S, State[S, B].
    */
  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s1) = run(s) //execute the run state action function with the type s, returns a tuple two of type (A, S).
    f(a).run(s1) //execute function f(a) that result in a type State(S, A), then execute run with the State s1, which results in a tuple two (A, S)
  })

  /**
    * The get action simply passes the incoming state along and returns it as the value.
    *
    * @tparam S incoming state
    * @return returns the state as a tuple two with the state s as a value and the state s.
    */
  def get[S]: State[S, S] = State(s => (s, s))

  /**
    * The set action is constructed with a new state s. The resulting action ignores the incoming state, replaces it with a
    * new state, an returns () instead of a meaningful value.
    *
    * @param s the new state.
    * @tparam S type parameter
    * @return a new state with () instead of a meaningful value.
    */
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

}

object State {
  type Rand[A] = State[RNG, A]


  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  /**
    * The modify function modifies the state in arbitrary ways, by using the get and set combinator functions.
    * This method returns a state action that modifies the incoming state by the the function f.
    * It yields Unit to indicate that is doesn't have a return value other than a State S.
    *
    * @param f
    * @tparam S type param
    * @return
    */
  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  // The idiomatic solution is expressed via foldRight
  def sequenceViaFoldRight[S, A](sas: List[State[S, A]]): State[S, List[A]] =
    sas.foldRight(unit[S, List[A]](List()))((f, acc) => f.map2(acc)(_ :: _))

  /**
    * The sequence function takes a list of transitions/state actions and combines them into
    * one single transition.
    *
    * This implementation uses a loop internally and is the same recursion
    * pattern as a left fold. It is quite common with left folds to build
    * up a list in reverse order, then reverse it at the end.
    * (We could also use a collection.mutable.ListBuffer internally.)
    *
    * @param sas a list of State transitions
    * @tparam S type parameter.
    * @tparam A type parameter.
    * @return a new single transition (state action).
    */
  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] = {
    def go(s: S, actions: List[State[S, A]], acc: List[A]): (List[A], S) =
      actions match {
        case Nil => (acc.reverse, s)
        case h :: t => h.run(s) match {
          case (a, s2) => go(s2, t, a :: acc)
        }
      }
    State((s: S) => go(s, sas, List()))
  }

  /**
    * The get action simply passes the incoming state along and returns it as the value.
    *
    * @tparam S incoming state
    * @return returns the state as a tuple two with the state s as a value and the state s.
    */
  def get[S]: State[S, S] = State(s => (s, s))

  /**
    * The set action is constructed with a new state s. The resulting action ignores the incoming state, replaces it with a
    * new state, an returns () instead of a meaningful value.
    *
    * @param s the new state.
    * @tparam S type parameter
    * @return a new state with () instead of a meaningful value.
    */
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

}

object SimpleRNGApp extends App {
  val Rng = SimpleRNG(42)
  println(s"> rng: $Rng")

  val (n1, rng2) = Rng.nextInt
  println(s"> n1: $n1, rng2: $rng2")

  val (n2, rng3) = rng2.nextInt
  println(s"> n1: $n2, rng2: $rng3")

  val (nv, nv2) = Rng.randomPair(Rng)
  println(s"> nv: $nv, nv2: $nv2")

  //Same state
  val (xs, rng4) = Rng.ints(10)(Rng)
  println(s"\n> xs: ${xs.toString()}")
  println(s"> rng4: ${rng4.nextInt}")
  println(s"> rng4: ${rng4.nextInt}\n")

  /**
    * Test of rollDie
    */
  val (dv, r) = Rng.rollDie(Rng)
  println(s"> Roll die value: $dv")

  /**
    * State action (combinator function) type int:
    * combinator for generating an Int using a RNG and
    * also transitions the RNG to a new state that can be used by another action
    * later (as given below: here the RNG generator 'r' is used later by 'res(r)'.
    */
  val res = Rng.int
  val (n, dr) = res(Rng)
  println(s"> State action res: $res")
  println(s"> State action res.n: $n")
  println(s"> State action res.r: $r\n")

  val res2 = Rng.int
  val (n_2, r_2) = res(r)
  println(s"> State action res: $res")
  println(s"> State action res.n: $n_2")
  println(s"> State action res.r: $r_2\n")

  /**
    * unit is a RNG state combinator.
    * It combines the Rand actions while avoiding explicitly passing along the RNG state.
    * It passes the RNG state through without using it, always returning a constant value rather than a random value:
    */
  val double = 10.0
  val res3 = Rng.unit(double)
  val (n_3, r_3) = res3(Rng)
  println(s"> State action/RNG state unit combinator: $res3")
  println(s"> State action/RNG state unit combinator: $n_3")
  println(s"> State action/RNG state unit combinator: $r_3\n")

  //Not using the passed in RNG, constant "random" value generated and same state of RNG object.
  val (n_4, r_4) = res3(Rng)
  println(s"> State action/RNG state unit combinator: $res3")
  println(s"> State action/RNG state unit combinator: $n_4")
  println(s"> State action/RNG state unit combinator: $r_4\n")

  //nonNegativeEven and nonNegativeEvenText
  /**
    * Testing combinator functions nonNegativeEven and nonNegativeEvenText
    * (that transforms the result from the state action, without modifying the state itself)
    */
  val (ne_5, re_5) = Rng.nonNegativeEven(Rng)
  println(s"> nonNegativeEven ne_5: $ne_5")
  println(s"> nonNegativeEven re_5: $re_5\n")

  val (nt_5, rt_5) = Rng.nonNegativeEvenText(Rng)
  println(s"> nonNegativeEvenText nt_5: $nt_5")
  println(s"> nonNegativeEvenText rt_5: $rt_5\n")

  //E.g. usage of nonNegativeEven function
  val (n_5, r_5) = Rng.nonNegativeEven(Rng)
  println(s"> nonNegativeEven n_5: $n_5")
  println(s"> nonNegativeEven r_5: $r_5\n")

  //Next state and random positive even number (positive and divisible by two)
  val (n_6, r_6) = Rng.nonNegativeEven(r_5)
  println(s"> nonNegativeEven n_6: $n_6")
  println(s"> nonNegativeEven r_6: $r_6\n")

  //Next state and random positive even number (positive and divisible by two)
  val (n_7, r_7) = Rng.nonNegativeEven(r_6)
  println(s"> nonNegativeEven n_7: $n_7")
  println(s"> nonNegativeEven r_7: $r_7\n")

  //Using doubleViaMap function
  val res4 = Rng.doubleViaMap(Rng)
  //Returns a Rand[Double] that is  type equal to RNG => (A, RNG)
  val (n_8, r_8) = res4(Rng)
  println(s"> Rng.doubleViaMap n_8: $n_8")
  println(s"> Rng.doubleViaMap r_8: $r_8")

}
