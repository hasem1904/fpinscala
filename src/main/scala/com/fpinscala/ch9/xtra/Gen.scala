package com.fpinscala.ch9.xtra

import com.fpinscala.ch9.xtra.RNG.RandF


//--------------------------------------------------------------------------
/**
  * https://github.com/malliina/fpinscala/blob/master/src/main/scala/com/mle/fpis/Gen.scala
  * @author Michael
  */

case class Gen[A](sample: State[RNG, A]) {
  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  def map[B](f: A => B): Gen[B] =
    Gen(sample.map(f))

  def map2[B, C](g: Gen[B])(f: (A, B) => C): Gen[C] =
    Gen(sample.map2(g.sample)(f))

  /*def unit[A](a: => A): Gen[A] =
    Gen(State.unit(a))*/

  /*def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(Gen.listOfN(_, this))

  def unsized: SGen[A] = SGen(i => this) // clearly bogus
  */

  def **[B](g: Gen[B]): Gen[(A, B)] =
    (this map2 g)((_, _))
}

object Gen {
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(between(start, stopExclusive)))

  //def unit[A](a: => A): Gen[A] = Gen(State.unit(a))
  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] = Gen(State(RNG.intf).map(_ < 0))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State(RNG.sequence(List.fill(n)(g.sample.run))))

  def sameParity(from: Int, to: Int): Gen[(Int, Int)] =
    choose(from, to + 1).flatMap(first => {
      val predicate: Int => Boolean = if (first % 2 == 0) _ % 2 == 0 else _ % 2 != 0
      Gen(State(having(predicate)).map(second => (first, second)))
    })

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(b => if (b) g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val (ga, weightA) = g1
    val (gb, weightB) = g2
    val g1Share = weightA / (weightA + weightB)
    choose(0, Integer.MAX_VALUE).flatMap(value => {
      if (value < g1Share * Integer.MAX_VALUE) ga else gb
    })
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Streams.Stream[A] =
    Streams.Stream.infiniteUnfold(rng)(g.sample.run)

  private def between(start: Int, stopExclusive: Int): RandF[Int] = rng => {
    val range = stopExclusive - start
    if (range < 0) {
      val (i, state) = rng.nextInt
      if (i >= start && i < stopExclusive) (i, state)
      else between(start, stopExclusive)(state)
    } else {
      RNG.map(RNG.positiveLessThan(range))(_ + start)(rng)
    }
  }

  private def betweenInefficient(start: Int, stopExclusive: Int): RandF[Int] = rng => {
    val (i, state) = rng.nextInt
    if (i >= start && i < stopExclusive) (i, state)
    else betweenInefficient(start, stopExclusive)(state)
  }

  private def having(p: Int => Boolean): RandF[Int] = rng => {
    val (i, state) = rng.nextInt
    if (p(i)) (i, state)
    else having(p)(state)
  }
}

