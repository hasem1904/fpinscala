package com.fpinscala.ch12

import java.util.Date

/**
  * This class represents the WebForm example in 'FP in Scala'.
  * Xtra, @see https://github.com/facaiy/book_notes/tree/master/Manning_Functional_Programming_in_Scala/src/main/scala/io/github/facaiy/fp/scala/c12
  */
case class WebForm(name: String, birthDate: Date, phoneNumber: String)

object WebForm extends App {
  def validName(name: String): Validation[String, String] =
    if (name.isEmpty) Failure("Name cannot be empty.")
    else Success(name)

  def validBirthdate(birthdate: String): Validation[String, Date] =
    try {
      import java.text._
      Success(new SimpleDateFormat("yyyy-MM-dd").parse(birthdate))
    } catch {
      case _: Throwable => Failure("Birthdate must be in the form yyyy-MM-dd.")
    }

  def validPhone(phoneNumber: String): Validation[String, String] =
    if (phoneNumber.matches("[0-9]{10}")) Success(phoneNumber)
    else Failure("Phone number must be 10 digits.")

  def validWebForm(name: String, birthdate: String, phoneNumber: String): Validation[String, WebForm] =
    // Using validationApplicative, exercise 12.6
    Applicative.validationApplicative[String].map3(
      validName(name),
      validBirthdate(birthdate),
      validPhone(phoneNumber)
    )(WebForm(_, _, _))
}
