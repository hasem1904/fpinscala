package com.fpinscala.ch12

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
  * Simple test related to WebFormTest class.
  *
  * Xtra: @see https://github.com/facaiy/book_notes/blob/master/Manning_Functional_Programming_in_Scala/src/test/
  */
@RunWith(classOf[JUnitRunner])
class WebFormTest extends org.scalatest.FunSuite {
  test("Validation of WebForm with invalid fields") {
    assert(WebForm.validWebForm("", "19.04.76", "119") ===
      Failure("Name cannot be empty.", Vector("Birthdate must be in the form yyyy-MM-dd.", "Phone number must be 10 digits.")))
  }

  test("Validation of WebForm with valid fields") {
    import java.util.{Calendar, GregorianCalendar}
    val calendar = new GregorianCalendar(1976, Calendar.APRIL, 19)
    assert(WebForm.validWebForm("Håvard", "1976-04-19", "1234567891") ===
      Success(WebForm("Håvard", calendar.getTime,"1234567891")))
  }
}
