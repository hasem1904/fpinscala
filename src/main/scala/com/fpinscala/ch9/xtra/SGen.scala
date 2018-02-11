package com.fpinscala.ch9.xtra

/**
  * https://github.com/malliina/fpinscala/blob/master/src/main/scala/com/mle/fpis/SGen.scala
  *
  * @author Michael
  */
/*
case class SGen[A](forSize: Int => Gen[A])

object SGen {
  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(i => Gen.listOfN(i, g))

  def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen(i => Gen.listOfN(i + 1, g))
}
*/
