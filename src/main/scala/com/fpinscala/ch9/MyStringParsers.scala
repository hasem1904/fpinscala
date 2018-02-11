package com.fpinscala.ch9

/**
  * Created by havard on 5/15/17.
  */

object MyStringParser {
  //type Parser[Char] = Char => Either[ParseError, Char]
  type Parser[+A] = String => Either[ParserError, String]
}


/*
class MyStringParsers extends com.fpinscala.ch9.Parsers[ParserError, MyStringParser.Parser] {

  //override def run[A](p: Parser[MyStringParser.Parser])(input: String): Either[ParserError, String] =

  override def run[A](p: Parser[ParserError, A])(input: String): Either[ParserError, String] = ???


  /*def Char(c: String): Parser[String] =
    (input: String) =>
      if ("a" == c) Right(c)
      else Left(ParserError(s"Error parsing char! Char $c is not an 'a'"))*/
  //If if succeeds we want the result, otherwise we expect information about the failure.
}
*/
