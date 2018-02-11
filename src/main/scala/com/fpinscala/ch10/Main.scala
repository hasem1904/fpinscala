package com.fpinscala.ch10

object Main extends App {
  /** Define a max function */
  val max = (x: Int, y: Int) => if (x < y) y else x

  /** Define an function of the Function2 type (takes two int parameters and returns an int)
    * Define the max function */
  val anonfun2 = new Function2[Int, Int, Int] {
    /** Running the Function2 type with apply function */
    def apply(x: Int, y: Int): Int = if (x < y) y else x
  }

  println(s"> max(0, 1):      ${max(0, 1)}")
  //Creating a curried version of the max function.
  val cf = max.curried
  val result = cf(3)
  println(s"> result: ${result(15)}")

  println(s"> anonfun2(0, 1): ${anonfun2(0, 1)}")
  assert(max(0, 1) == anonfun2(0, 1))
}
