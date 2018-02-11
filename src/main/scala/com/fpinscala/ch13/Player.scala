package com.fpinscala.ch13

case class Player(name: String, score: Int)

object Player extends App {

  def Println(msg: String): IO[Unit] =
    new IO[Unit] {
      override def run: Unit = println(msg)
    }

  def contest(p1: Player, p2: Player): Unit = {
    if (p1.score > p2.score)
      println(s"${p1.name} is the winner")
    if (p2.score > p1.score)
      println(s"${p2.name} is the winner")
    else
      println("It's a draw")
  }

  def winner(p1: Player, p2: Player): Option[Player] =
    if (p1.score > p2.score) Some(p1)
    else if (p2.score > p1.score) Some(p2)
    else None


  def contest2(p1: Player, p2: Player): Unit = winner(p1, p2) match {
    case Some(Player(name, _)) => println(s"$name is the winner")
    case None => println("It's a draw.")
  }

  def winnerMsg(p: Option[Player]): String = p map {
    case Player(name, _) => s"$name is the winner"
  } getOrElse "It's a draw."

  def contest3(p1: Player, p2: Player): Unit =
  //println(winnerMsg(winner(p1, p2)))
    Println(winnerMsg(winner(p1, p2)))

  /**
    * The IO Monad:
    *
    * The IO type now allows input by specifying the type parameter A
    *
    * @tparam A
    */
  trait IO[A] {
    self =>

    def run: A

    def map[B](f: A => B): IO[B] =
      new IO[B] {
        def run = f(self.run)
      }

    def flatMap[B](f: A => IO[B]): IO[B] =
      new IO[B] {
        def run = f(self.run).run
      }
  }
}
