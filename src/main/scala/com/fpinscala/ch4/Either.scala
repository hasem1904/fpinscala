package com.fpinscala.ch4

/**
  * Created by havard on 1/28/17.
  */

sealed trait Either[+E, +A] {
  /** Exercise 4.6 */
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(a) => Right(f(a)) //Success: Transform type A to type B and add it in the Right container
    case Left(a) => Left(a) //Failure: put it in the Left container
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => f(a)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_) => b
    case Right(a) => Right(a)
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {
      a <- this
      bb <- b
    } yield f(a, bb)

  /** Exercise 4.7 */
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es)(x => x)

  def traverse[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = es match {
    case Nil => Right(Nil)
    //Calling function f(x) gives a Either[E, B] type that we can run map2 function on.
    //The map2 function takes the input type (b: Either[EE, B])(f: (A, B) => C),the traverse(xs)(f) serves as the
    //input arguments to map2 function. Note we traverse the list by calling the tail of the list, traverse(xs)(f).
    //Then the last argument function to traverse takes the two closest element in the list, and combines them two a new Either[E, B] type.
    case x :: xs => (f(x).map2(traverse(xs)(f)))(_ :: _)
  }
}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]

//Companion object
object Either {
  def mean(xs: IndexedSeq[Double]): Either[String, Double] = {
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)
  }

  def safeDiv(x: Int, y: Int): Either[Exception, Int] = {
    try Right(x / y)
    catch {
      case e: Exception => Left(e)
    }
  }

  def Try[A](a: => A): Either[Exception, A] = {
    try Right(a)
    catch {
      case e: Exception => Left(e)
    }
  }

  /**
    * Top secret formula for computing an annual car insurance premium from
    * two key factors
    *
    * @param age
    * @param numberOfSpeedingTickets
    * @return
    */
  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = {
    age / numberOfSpeedingTickets
  }

  /**
    * Either can be used in for-comprehension, e.g.:
    */
  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Either[Exception, Double] = {
    for {
    // Single argument to function cat be written with {}, same as Try(age.toInt).
      a <- Try {
        age.toInt
      }
      tickets <- Try {
        numberOfSpeedingTickets.toInt
      }
    } yield insuranceRateQuote(a, tickets)
  }
}

object AppEither extends App {
  println(s"> mean of empty list: ${Either.mean(IndexedSeq(Double.NaN))}")
  println(s"> mean list: ${Either.mean(IndexedSeq(1.0, 3.6, 8.9, 3.5, 9.9))}")
  println(s"> safeDiv: ${Either.safeDiv(15, 3)}")
  println(s"> safeDiv: ${Either.safeDiv(15, 0)}")
}