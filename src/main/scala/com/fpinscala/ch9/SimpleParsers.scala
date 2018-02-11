package com.fpinscala.ch9

/**
  * Parser[+_] is a type parameter that itself is a covariant type constructor.
  * This interface will work for any representation of Parser.
  *
  *
  * Created by havard on 5/14/17.
  */
trait Parser[ParseError, Parser[+_]] {
  //If if succeeds we want the result, otherwise we expect information about the failure.
  def run[A](p: Parser[A])(c: String): Either[ParseError, A]
  def char(c: Char): Parser[Char]
}
case class ParseError(msg: String)

class CharParser /*extends Parser*/{
  type myParser[Char] = Char => Either[ParserError, Char]

  //val P:Parser[Char]
  //import P._

  /*def Char(c: Char): Parser[Char] =
    (input: Char) =>
      if ('a' == c) Right(c)
      else Left(ParserError(s"Error parsing char! Char $c is not an 'a'"))

  P.run(char('c'))('c'.toString)*/

  //val res = P.run(char('c'))("Test")
  //println(res)
}


/*
class SimpleParsers[Char] extends Parser {

  type Parser[Char] = Char => Either[ParseError, Char]

  //char combinator for the task 'char'
  /*override def char(c: Char): Parser[Char] =
    (input: Char) =>
      if ('a' == c) Right(c)
      else Left(ParseError(s"Error parsing char! Char $c is not an 'a'"))*/

   /*override def run[A](p: Parser[A])(c: Char): Either[ParseError, A] =
   run(p)(c)*/

  /*override def run[A](p: Parser[A])(c: Char): Either[ParseError, A] = p(c)*/
  //If if succeeds we want the result, otherwise we expect information about the failure.

}
*/
