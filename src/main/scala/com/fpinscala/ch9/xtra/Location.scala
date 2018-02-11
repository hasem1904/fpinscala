package com.fpinscala.ch9.xtra

/*
import com.fpinscala.ch9.ParserError

/**
  * Created by havard on 5/15/17.
  */
case class Location(input: String, offset: Int = 0) {
  private def failureSlice = input.slice(0, offset - 1)

  lazy val line = failureSlice.count(_ == '\n') + 1
  lazy val col = failureSlice.reverse.indexOf('\n')

  def toError(msg: String): ParseError = ParserError(List((this, msg)))

  def fromOffset = input substring offset

  def advanceBy(n:Int) = copy(offset = offset+n)
}
*/
