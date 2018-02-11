package com.fpinscala.ch9.xtra

import scala.util.matching.Regex

/**
  * Created by havard on 5/15/17.
  */
/*
trait Parsers[Parser[+ _]] {
  def run[A](p: Parser[A])(input: Location): com.fpinscala.ch9.xtra.Parsers.Result[A]

  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]

  def succeed[A](a: A): Parser[A] = string("") map (_ => a)

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  def slice[A](p: Parser[A]): Parser[String]

  def label[A](msg: String)(p: Parser[A]): Parser[A]

  def scope[A](msg: String)(p: Parser[A]): Parser[A]

  def attempt[A](p: Parser[A]): Parser[A]

  implicit def regex(r: Regex): Parser[String]

  implicit def string(s: String): Parser[String]

  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))

  def digit: Parser[Int] = regex("[0-9]".r).map(_.toInt)

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n == 0) succeed(Nil)
    else map2(p, listOfN(n - 1, p))(_ :: _)

  def product[A, B](p1: Parser[A], p2: Parser[B]): Parser[(A, B)]

  def map2[A, B, C](p1: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    product(p1, p2).map(f.tupled)

  def map2InTermsOfFlatMap[A, B, C](p1: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    p1.flatMap(a => p2.map(b => f(a, b)))

  case class ParserOps[A](p: Parser[A]) {
    def self = Parsers.this

    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def |[B >: A](p2: => Parser[B]): Parser[B] = p or p2

    def many: Parser[List[A]] = map2(p, p.many)(_ :: _) or succeed(Nil)

    def many1: Parser[List[A]] = map2(p, many)(_ :: _)

    /**
      * @return a parser that parses according to this parser followed by `p2`, assuming this parser is successful.
      */
    def product[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)

    def productInTermsOfFlatMap[B](p2: Parser[B]): Parser[(A, B)] =
      p.flatMap(a => p2.map(b => (a, b)))

    def **[B](p2: Parser[B]) = product(p2)

    def map[B](f: A => B): Parser[B] = p.flatMap(a => succeed(f(a)))

    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)

    /**
      * @return a parser that returns the portion of the input string matched by the parser; otherwise like `many`
      */
    def slice: Parser[String] = self slice p
  }

  val numA = char('a').many.map(_.size)
  val numA2 = char('a').many.slice.map(_.size)
  val aThenOneOrMoreB = char('a').many.slice.map(_.size) ** char('b').many1.slice.map(_.size)

  val thatManyCharacters = {
    val digitParser = regex("[0-9]".r)
    digitParser ** digitParser.flatMap(s => listOfN(s.toInt, char('a')))
  }

  //  def errorLocation(e: ParseError): Location
  //
  //  def errorMessage(e: ParseError): String

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      Prop.forAll(in)(s => {
        val loc = Location(s, 0)
        run(p1)(loc) == run(p2)(loc)
      })

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)

    // def succeedLaw[A](p: Parser[A])(in: Gen[String]): Prop =
    // Prop.forAll(in)(s => run(succeed(b)) == Right(b))
  }

}

object Parsers {

  trait Result[+A] {
    def mapError(f: ParseError => ParseError): Result[A] = this match {
      case Failure(error, isCommitted) => Failure(f(error), isCommitted)
      case _ => this
    }

    def uncommit: Result[A] = this match {
      case Failure(error, committed) if committed => Failure(error, isCommitted = false)
      case _ => this
    }

    def addCommit(isCommitted: Boolean): Result[A] = this match {
      case Failure(error, false) if isCommitted => Failure(error, true)
      case _ => this
    }
  }

  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]

  case class Failure(get: ParseError, isCommitted: Boolean) extends Result[Nothing]

}
*/
