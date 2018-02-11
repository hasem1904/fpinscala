package com.fpinscala.ch4

/**
  * Created by havard on 1/22/17.
  */

/**
  * Functions that is provided by the Option trait.
  * Functions are placed inside the Option trait, for achieving syntax like: 'obj.fn(arg)' or 'obj fn arg'
  *
  * @tparam A the provided specific type.
  */
trait Option[+A] {
  //Exercise 4.1:
  //Apply f if Option is not 'None'
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  //Apply f which may fail to the Option if not None
  def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None

  def flatMap1[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }

  //For option composition, lifting and wrapping exception-oriented API
  //Any function that we already have lying around, can be transformed via lift
  //to operate within the context of a single Option value.
  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  //E.g.: math.abs is now able to work with Option values.
  //Lifted the math.abs to work with Option values
  val abs0: Option[Double] => Option[Double] = lift(math.abs)

  //The 'B >:A' says that the 'B' type parameter must be a super type of 'A'
  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  //Don't evaluate 'ob' unless needed
  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case Some(a) => this
  }

  //Convert 'Some' to 'None' if the value doesn't satisfy f
  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => this
    case _ => None
  }

  def filter1(f: A => Boolean): Option[A] =
    flatMap(a => if (f(a)) Some(a) else None)
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

//Option companion object
object Option {

  /** Exercise 4.3 */
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap (aa => b map (bb => f(aa, bb)))

  /** Exercise 4.4 */
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    a match {
      // If list is empty, return an Option with empty list.
      case Nil => Some(Nil)
      //If one element exists in the list, run flatMap on Option[A] element.
      //flatMap(f:(A) => Option[B]), meaning taking an input argument a of type A and
      //and returning an Option[B] type. Here: the tail of the list xs, is processed by sequence
      //recursively that returns an Option[List[A]]. Each element in Option[List[A]], is then transformed by
      //map[B](f: A => B): Option[B] to satisfy the return type of flatMap(f:(A) => Option[B]) function.
      case x :: xs => x flatMap (a => sequence(xs).map(
        //Adds an element at the beginning of this list:
        //def ::[B >: A] (x: B): List[B]
        {
          println(s"> Adding element to list : $a")
          //a ::(_)
          a :: _
        }
      ))
    }
  }

  def parseInts(xs: List[String]): Option[List[Int]] = sequence(xs map (i => Try(i.toInt)))

  /** Exercise 4.5 */
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    //def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C]
    //
    //map2 takes one argument of Option[A], the function f takes one element of type A and return an Option[B].
    //The seconds argument of map2, takes of Option[B], the traverse function, traverses the rest of the list/tail of
    //the list with its function f (for transforming the next element in the list, from a type A to Option[B].
    //Then the last argument function to traverse takes the two closest element in the list, and combines them two a new Option[C] type.
    //Calling traverse: (a: List[A]) = (map2(f(x), traverse(xs)(f))) and (f: A => Option[B]) = (_ :: _).
    case x :: xs =>(map2(f(x), traverse(xs)(f)))(_ :: _)
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

  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Option[Double] = {
    val optAge: Option[Int] = Try {age.toInt}
    val optTickets: Option[Int] = Try {numberOfSpeedingTickets.toInt}
    //Lifting insuranceRateQuote with map2 to work with Option values.
    map2(optAge, optTickets)(insuranceRateQuote)
  }

  def Try[A](a: => A): Option[A] = {
    try Some(a)
    catch {
      case e: Exception => None
    }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  /**
    * Exercise 4.2:
    * Variance function in terms of flatMap.
    * The mean of a sequence is m, the variance is the mean of
    * math.pow(x-m, 2) for each element x in the sequence.
    *
    * @param xs
    * @return
    */
  def variance(xs: Seq[Double]): Option[Double] = {
    //Get the mean of a sequence: mean(xs).
    //Use flatMap to return 'Some(a) of another type' result or 'None' if variance can't be calculated.
    //Must use xs.map to calculate the variance of all element in the sequence xs.
    //Then get the variance of each "element mean" in the sequence.
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))
  }
}

object OptionApp {
  def main(args: Array[String]): Unit = {
    //Testing functions...
    val data = Seq[Double](1.0, 3.6, 2.1, 7, 9)
    println(s"variance: ${Option.variance(data)}")
    println(s"Lifting parseInsuranceRateQuote: ${Option.parseInsuranceRateQuote("123", "12")}")
    println(s"Lifting parseInsuranceRateQuote: ${Option.parseInsuranceRateQuote("123ph", "12")}")
    println(s"parseInts: ${Option.parseInts(List("123", "12"))}")
  }
}
