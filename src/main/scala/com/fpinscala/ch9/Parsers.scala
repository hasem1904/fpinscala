package com.fpinscala.ch9

/**
  * Parser trait
  * Parser[+ _]: Parser is a type parameter that itself is a covariant type constructor
  *
  * @tparam Parser
  */
trait Parsers[ParserError, Parser[+ _]] {
  /** Introduces the name 'self' to refer to this Parsers instance (Used later in ParserOps) */
  self =>

  def run[A](p: Parser[A])(input: String): Either[ParserError, A]

  def char(c: Char): Parser[Char] //Here the Parser type constructor is applied to Char.

  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)

  /**
    * Note:
    * Making string an implicit conversion and added another implicit asStringParser.
    * With this two functions, Scala will automatically promote String to a Parser and we get infix operators
    * for any type that can be converted to a Parser[String]. So given: val P:Parser, we can then:
    * import P._
    * to let us write expressions like: "abra" | "cadabra" to create parsers. This will work for all
    * implementations of Parsers. Other binary operators or methods can be added to the body of ParserOps.
    */
  implicit def string(s: String): Parser[String]

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String])

  def orString(s1: String, s2: String): Parser[String]

  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]

  /**
    * Parser options for running parser functions with optional syntax.
    *
    * @param p the parser
    * @tparam A parser type
    */
  case class ParserOps[A](p: Parser[A]) {
    //Using 'self' to explicitly disambiguate reference to the or method on the trait
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def or[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
  }
}



/**
  * Parser trait.
  * Introduces the name 'self' to refer to this Parsers instance; It's
  * used by 'ParserOps' class. (So inner classes may call methods of trait)
  *
  * See: https://github.com/fpinscala-muc/fpinscala/tree/master/exercises/src/test/scala/fpinscala/parsing
  *
  * Created by havard on 5/2/17.
  */
/*
trait Parsers[ParserError, Parser[+_]] { self => //So inner classes can call method of trait
  def run[A](p: Parser[A])(input: String): Either[ParserError, A]

  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]):

  //ParserOps[String] = ParserOps(f(a))

  def many[A](p:Parser[A]):Parser[List[A]]

  def map[A,B](a:Parser[A])(f:A => B):Parser[B]

  def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))

  def orString(s1: String, s2: String): Parser[String]

  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]

  /*
   * A default `succeed` implementation in terms of `string` and `map`.
   * We leave `succeed` abstract, since `map` is defined below in terms of
   * `flatMap` and `succeed`, which would be a circular definition! But we include
   * the definition here in case implementations wish to use it
   * (say if they provide a custom implementation of `map`, breaking the cycle)
   */
  /*def defaultSucceed[A](a: A): Parser[A] =
    string("") map (_ => a)*/

  /*def succeed[A](a: A): Parser[A]*/

  /*case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >: A](p2: => Parser[B]):Parser[B] = self.or(p, p2)
  }*/

 object Laws{
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
    forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p:Parser[A])(in:Gen[String]):Prop =
    equal(p,p.map(a=> a))(in)

  }

//  def map[A,B](a: Parser[A])(f: A => B): Parser[B] =
//  flatMap(a)(f andThen succeed)
}

object Parsers{
  //type Parser[+A] = String =>
}*/


