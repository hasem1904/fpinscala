package com.fpinscala.ch4

/**
  * Created by havard on 1/29/17.
  */
case class Person(name: Name, age: Age)
sealed class Name(val value: String)
sealed class Age(val value: Int)

object Person extends App {
  def mkName(name: String): Either[String, Name] =
    if (name == "" || name == null) Left("Name is empty.")
    else Right(new Name(name))

  def mkAge(age: Int): Either[String, Age] =
    if (age < 0) Left("Age is out of range")
    else Right(new Age(age))

  /** Note: Definition on map2, map2 works on the Either type:
    * def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C]
    * Here:
    * (b: Either[EE, B]) = (mkAge(age))
    * (f: (A, B) => C) = (Person(_, _)) = (a, b) => Person (a, b)
    * */
  def mkPerson(name: String, age: Int): Either[String, Person] =
    //mkName(name).map2(mkAge(age))(Person(_, _))
    mkName(name).map2(mkAge(age))((a,b) => Person(a, b))
}
