package com.fpinscala.ch12

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
  * Tests
  * Xtra:
  * @see https://github.com/fpinscala/fpinscala/blob/master/answers/src/main/scala/fpinscala/applicative/Applicative.scala
  * @see https://github.com/facaiy/book_notes/blob/master/Manning_Functional_Programming_in_Scala/src/test/scala/io/github/facaiy/fp/scala/c12/ApplicativeSuite.scala
  */
@RunWith(classOf[JUnitRunner])
class ApplicativeTest extends org.scalatest.FunSuite {
  /**
    * When writing tests, one would often like to re-use certain values for multiple
    * tests. For instance, we would like to create an Int-set and have multiple test about it.
    *
    * Instead of copy-pasting the code for creating the set into every test, we can
    * store it in the test class using a val:
    *
    * val s1 = singletonSet(1)
    *
    * However, what happens if the method "singletonSet" has a bug and crashes?
    * Then the test methods are not even executed, because creating an instance of the
    * test class fails!
    *
    * Therefore, we put the shared values into a separate trait (traits are like
    * abstract classes), and create an instance inside each test method.
    */
  trait ApplicableTest {
    /** Example data for 'Option Applicative' */
    val depts: Map[String, String] = Map("Peter" -> "Finance", "Bob" -> "Sales")
    val salaries: Map[String, Double] = Map("Peter" -> 200, "Bob" -> 450)

    /** Example code of 'Option monad' */
    val idsByName: Map[String, Int] = Map("Peter" -> 1, "Bob" -> 2)
    val depts1: Map[Int, String] = Map(1 -> "Finance", 2 -> "Sales")
    val salaries1: Map[Int, Double] = Map(1 -> 200, 2 -> 450)

    /** Example code of 'Option applicative' */
    val F: Applicative[Option] = new Applicative[Option] {
      override def unit[A](a: => A): Option[A] = Some(a)
      override def map2[A, B, C](fa: Option[A], fb: Option[B])(f: (A, B) => C): Option[C] = (fa, fb, f) match {
        case (Some(a), Some(b), _) => Some(f(a, b))
        case _ => None
      }
    }
  }

  /** Testing the 'Option Applicative' */
  test("Option Applicative should return expected string") {
    new ApplicableTest {
      val expOptApplicable: String = "Peter in Finance makes 200.0 per year."
      val actOptApplicable: Option[String] =
      //Peter in Finance makes 200.0 per year.
        F.map2(depts.get("Peter"), salaries.get("Peter")) {
          (dept, salary) => s"Peter in $dept makes $salary per year."
        }
      assert(expOptApplicable === actOptApplicable.get)
    }
  }

  /** Testing the 'Option Monad' */
  test("Option Monad should return expected string") {
    new ApplicableTest {
      val expOptMonad: String = "Bob in Sales makes 450.0 per year."
      val actOptMonad: Option[String] = idsByName.get("Bob").flatMap { id =>
        F.map2(depts1.get(id), salaries1.get(id)) {
          (dept, salary) => s"Bob in $dept makes $salary per year."
        }
      }
      assert(expOptMonad === actOptMonad.get)
    }
  }
}
