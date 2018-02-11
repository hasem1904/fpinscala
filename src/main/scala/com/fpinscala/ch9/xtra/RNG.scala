package com.fpinscala.ch9.xtra

/**
  * Created by havard on 5/15/17.
  */
/**
  *
  * @author mle
  */
trait RNG {
  def nextInt: (Int, RNG)
}
object RNG {
  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = Simple(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  def positiveInt(rng: RNG): (Int, RNG) = {
    val (value, state) = rng.nextInt
    // should I accept 0 as positive?
    if (value == 0 || value == Int.MinValue) positiveInt(state)
    else if (value < 0) (-value, state)
    else (value, state)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (value, state) = positiveInt(rng)
    (1.0D - value.toDouble / Int.MaxValue, state)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, state1) = rng.nextInt
    val (d, state2) = double(state1)
    ((i, d), state2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val pair = intDouble(rng)
    (pair._1.swap, pair._2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, s1) = double(rng)
    val (d2, s2) = double(s1)
    val (d3, s3) = double(s2)
    ((d1, d2, d3), s3)
  }

  def intsF(count: Int)(rng: RNG): (List[Int], RNG) = {
    def intsAcc(n: Int, state: RNG, acc: List[Int]): (List[Int], RNG) =
      if (n == 0) (acc, state)
      else {
        val (i, s) = state.nextInt
        intsAcc(n - 1, s, i :: acc)
      }
    intsAcc(count, rng, Nil)
  }

  def ints(count: Int): Rand[List[Int]] = {
    def intsAcc(n: Int, state: RNG, acc: List[Int]): (List[Int], RNG) =
      if (n == 0) (acc, state)
      else {
        val (i, s) = state.nextInt
        intsAcc(n - 1, s, i :: acc)
      }
    State(rng => intsAcc(count, rng, Nil))
  }

  type RandF[+A] = RNG => (A, RNG)
  type Rand[+A] = State[RNG, A]

  val intf: RandF[Int] = _.nextInt
  val int: Rand[Int] = State(_.nextInt) //_.nextInt

  def unit[A](a: A): RandF[A] =
    rng => (a, rng)

  def map[A, B](s: RandF[A])(f: A => B): RandF[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def doubleElegantly: RandF[Double] =
    map(positiveInt)(i => 1.0D - i.toDouble / Int.MaxValue)

  def map2[A, B, C](ra: RandF[A], rb: RandF[B])(f: (A, B) => C): RandF[C] =
    rng => {
      val (a, sa) = ra(rng)
      val (b, sb) = rb(sa)
      (f(a, b), sb)
    }

  def both[A, B](ra: RandF[A], rb: RandF[B]): RandF[(A, B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: RandF[(Int, Double)] =
    both(intf, double)

  val randDoubleInt: RandF[(Double, Int)] =
    both(double, intf)

  def sequence[A](fs: List[RandF[A]]): RandF[List[A]] = {
    def seqAcc(rands: List[RandF[A]], state: RNG, acc: List[A]): (List[A], RNG) =
      rands match {
        case h :: t =>
          val (v, s) = h(state)
          seqAcc(t, s, v :: acc)
        case Nil =>
          (acc, state)
      }
    rng => seqAcc(fs, rng, Nil)
  }

  // RandF[Stream[A]] = RNG => (Stream[A], RNG)
  def randomStream[A](g: RandF[A])(rng: RNG): Streams.Stream[A] =
    Streams.Stream.infiniteUnfold(rng)(g)

  def intsUsingSequence(count: Int): RandF[List[Int]] =
    sequence(List.fill(count)(intf))

  def flatMap[A, B](f: RandF[A])(g: A => RandF[B]): RandF[B] =
    rng => {
      val (a, s) = f(rng)
      val rb = g(a)
      rb(s)
    }

  def positiveLessThan(n: Int): RandF[Int] =
    flatMap(positiveInt)(i => rng => {
      val mod = i % n
      // retry if the positive Int is higher than the largest multiple of n that fits in a 32-bit Int
      if (i + (n - 1) - mod > 0) (mod, rng) else positiveLessThan(n)(rng)
    })

  def mapUsingFlatMap[A, B](s: RandF[A])(f: A => B): RandF[B] =
    flatMap(s)(a => rng => (f(a), rng))

  def map2usingFlatMap[A, B, C](ra: RandF[A], rb: RandF[B])(f: (A, B) => C): RandF[C] =
    flatMap(ra)(a => rngA => {
      val (b, rngB) = rb(rngA)
      (f(a, b), rngB)
    })

  def erroneousRollDie: RandF[Int] = positiveLessThan(6)

  def rollDie: RandF[Int] = map(positiveLessThan(6))(_ + 1)

  def mapGeneral[S, A, B](fa: S => (A, S))(f: A => B): S => (B, S) =
    state => {
      val (a, sa) = fa(state)
      (f(a), sa)
    }

  //  type State[S, +A] = S => (A, S)

  sealed trait Input

  case object Coin extends Input

  case object Turn extends Input

  case class Machine(locked: Boolean, candies: Int, coins: Int)

  trait CandyDispenser {
    /**
      * @return ((coins, candies), machine)
      */
    def input(in: Input): ((Int, Int), Machine)
  }

  case class SimpleCandyDispenser(m: Machine) extends CandyDispenser {
    def input(in: Input): ((Int, Int), Machine) = in match {
      case Coin if m.locked => ((m.coins + 1, m.candies), m.copy(locked = false, coins = m.coins + 1))
      case Turn if !m.locked => ((m.coins, m.candies - 1), m.copy(locked = true, candies = m.candies - 1))
      case _ => ((m.coins, m.candies), m)
    }
  }

  type CandyState = State[CandyDispenser, Input]

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    val states: List[State[Machine, (Int, Int)]] =
      inputs.map(in => State[Machine, (Int, Int)](m => SimpleCandyDispenser(m).input(in)))
    State.sequence(states).map(_.last)
  }
}
