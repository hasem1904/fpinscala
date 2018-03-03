package com.fpinscala.ch13

class FahrenheitToCelsius{
  import IO1._

  /** Org. code */
  def fahrenheitToCelsius(f: Double): Double =
    (f - 32) * 5.0 / 9.0

  /** The converter method WITH side effects*/
  def converter: Unit = {
    println("Enter the temperature in degress Fahrenheit: ")
    val d = scala.io.StdIn.readLine.toDouble
    println(fahrenheitToCelsius(d))
  }

  /** Using the IO Monad */
  def ReadLine: IO[String] = IO{ scala.io.StdIn.readLine}
  def PrintLine(msg:String):IO[Unit] = IO1.IO{println(msg)}

  /** The converter method WITH NO side effects */
  def converterIO: IO[Unit] = for {
    _ <- PrintLine("Enter the temperature in degress Fahrenheit: ")
    d <- ReadLine.map(_.toDouble) //Using the monadic combinators map from IO Monad.
    _ <- PrintLine(fahrenheitToCelsius(d).toString)
  } yield ()

  /**
    * Other usages of the IO Monad:
    */
  val echo: IO[Unit] = ReadLine.flatMap(PrintLine) //IO[Unit]
  val readInt: IO[Int] = ReadLine.map(_.toInt)  //IO[Unit]
  //val readInts = readInt ** readInt  //IO[Unit]
  //val replicateM(10)(ReadLine) //IO[String] that reads 10 lines from the console
}

object FahrenheitToCelsius extends FahrenheitToCelsius with App{
  println("-------------------------------------------")
  println("> Running the Fahrenheit to Celsius program")
  println("-------------------------------------------")
  converterIO.run

  println("-------------------------------------------")
  println("> Running echo IO[Unit] Monad ")
  println("-------------------------------------------")
  echo.run
}
