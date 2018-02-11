package com.fpinscala.ch9.xtra

/**
  * @author Michael
  */
case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    State(s => {
      val (a, s2) = run(s)
      (f(a), s2)
    })

  def map2[B, C](other: State[S, B])(f: (A, B) => C): State[S, C] =
    State(s => {
      val (a, sa) = run(s)
      val (b, sb) = other.run(sa)
      (f(a, b), sb)
    })

  def flatMap[B](g: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, sa) = run(s)
      val sb = g(a)
      sb.run(sa)
    })
}

object State {
  def unit[A, S](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = {
    def seqAcc(rands: List[State[S, A]], state: S, acc: List[A]): (List[A], S) =
      rands match {
        case h :: t =>
          val (v, s) = h.run(state)
          seqAcc(t, s, acc :+ v)
        case Nil =>
          (acc, state)
      }
    State(s => seqAcc(fs, s, Nil))
  }
}
