package com.fpinscala.ch9.xtra

/**
  * Created by havard on 5/15/17.
  */
import java.util.concurrent.{ExecutorService, Executors}

/*
import com.fpinscala.ch9.xtra.Par.Par
import com.fpinscala.ch9.xtra.Prop._
*/

/**
  * @author Michael
  */
/*
case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  def &&(p: Prop): Prop = logic((thisResult, max, remainingTests, rng) => {
    if (thisResult.isEmpty) p.run(max, remainingTests, rng)
    else thisResult
  })

  def ||(p: Prop): Prop = logic((thisResult, max, remainingTests, rng) => {
    if (thisResult.isEmpty) thisResult
    else p.run(max, remainingTests, rng)
  })

  private def logic(f: (Result, MaxSize, TestCases, RNG) => Result): Prop = Prop((max, n, rng) => {
    val half = n / 2
    val incr = if (half + half < n) 1 else 0
    val thisResult = run(max, half, rng)
    f(thisResult, max, half + incr, rng)
  })
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int
  type Result = Option[(FailedCase, SuccessCount)]
  type MaxSize = Int

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop((max, n, rng) => {
    val randomIndexed = Streams.Stream.zip(Gen.randomStream(as)(rng), Streams.Stream.from(0)).take(n)
    randomIndexed.map {
      case (a, i) =>
        try {
          if (f(a)) None else Some((a.toString, i))
        } catch {
          case e: Exception => Some((buildMsg(a, e), i))
        }
    }.find(_.isDefined).getOrElse(None)
  })

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g.forSize)(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = (n + (max - 1)) / max
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop { (max, _, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max, n, rng)
  }

  def check(p: => Boolean): Prop = {
    lazy val result = p
    forAll(Gen.unit(()))(_ => result)
  }

  val S: Gen[ExecutorService] = Gen.weighted(
    Gen.choose(1, 4).map(Executors.newFixedThreadPool) -> .75,
    Gen.unit(Executors.newCachedThreadPool) -> .25
  )

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(S ** g) { case s ** a => f(a)(s).get()}

  def forAllPar2[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(S ** g) { case (s, a) => f(a)(s).get()}

  def forAllPar3[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(S.map2(g)((_, _))) { case (s, a) => f(a)(s).get()}

  def checkPar(p: Par[Boolean]): Prop =
    forAllPar(Gen.unit(()))(_ => p)

  def run(p: Prop,
          maxSize: Int = 100,
          testCases: Int = 100,
          rng: RNG = RNG.Simple(System.currentTimeMillis())) = {
    val message = p.run(maxSize, testCases, rng)
      .fold(s"+ OK, passed $testCases tests.")(pair => s"! Falsified after ${pair._2} passed tests:\n${pair._1}")
    println(message)
  }
}
*/
