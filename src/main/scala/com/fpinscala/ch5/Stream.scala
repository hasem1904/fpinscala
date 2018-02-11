package com.fpinscala.ch5

import com.fpinscala.ch5.Stream._

import scala.annotation.tailrec

/**
  * Created by havard on 1/30/17.
  */
trait Stream[+A] {

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  /**
    * exists2
    * Using foldRight non-strict function.
    * Here b is the unevaluated recursive step that folds the tail of the stream. If p(a)
    * returns true, b will never be evaluated and the computation terminated early.
    *
    * @param p predicate function
    * @return returns a boolean.
    */
  def exists2(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  /**
    * The foldRight using lazy val for head and tail.
    * (In essence, fold* takes data in one format and gives it back to you in another).
    * Applies the function f on between successive elements of the Stream, going from right to left,
    * starting with z.
    *
    * Note how the combining function f is non-strict in its second parameter.
    * If f chooses not to evaluate its second parameter, this terminates the traversal early
    * (the whole Stream will not be traversed!).
    *
    * @param z initial start value.
    * @param f function with type interference
    * @tparam B
    * @return
    */
  def foldRight[B](z: => B)(f: (A, => B) => B): B = //The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  /** Exercise 5.1
    * The natural recursive solution.
    * Note: will stack overflow for large streams, since it's not tail-recursive!!
    */
  def toList: List[A] = this match {
    case Cons(h, t) => h() :: t().toList
    case _ => Nil
  }

  /**
    * The above solution will stack overflow for large streams, since it's
    * not tail-recursive. Here is a tail-recursive implementation. At each
    * step we cons onto the front of the `acc` list, which will result in the
    * reverse of the stream. Then at the end we reverse the result to get the
    * correct order again.
    */
  def toList2: List[A] = {
    @tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h, t) => go(t(), h() :: acc)
      case _ => acc
    }
    go(this, Nil).reverse
  }

  /** Exercise 5.2 */
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1)) //Recursively calls next element on stream.
    case Cons(h, t) if n == 1 => cons(h(), empty) //Base case for traversing, creating a non empty stream.
    case _ => empty //Otherwise we return an empty stream.
  }

  @tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => t().drop(n - 1) //Recursively calls next element on stream.
    case _ => this //Otherwise we return an empty stream.
  }

  /** Exercise 5.3
    * Creates elements to the stream as long as the predicate function p holds.
    */
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => Empty
  }

  /** Exercise 5.4
    * Since `&&` is non-strict in its second argument, this terminates the traversal as soon as a none-matching element is found.
    */
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((h, t) => p(h) && t)

  /** Exercise 5.5
    *
    * Creates elements to the stream as long as the predicate function p holds.
    * */
  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) =>
      if (p(h)) cons(h, t) //Create an nonempty stream
      else empty)

  /** Exercise 5.6
    * Implement headOption using foldRight.
    * Should return the head element of the Stream in a Option type.
    * Note! we only call the 'h thunk' (by h() in foldRight), so only the head element in the Stream will be processed!
    */
  def headOption2: Option[A] =
    foldRight(None: Option[A])((h, _) => Some(h))

  /**
    * Exercise 5.7
    *
    * map, filter, append and flatMap using foldRight
    */
  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h, t) => cons(f(h), t)) //Construct element in Stream

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => if (f(h)) cons(h, t) //Create Stream element if predicate holds
    else t) //Otherwise nothing.

  def append[B >: A](s1: Stream[B]): Stream[B] =
    foldRight(s1)((h, t) => cons(h, t)) //Adding a Stream to this Stream

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, t) => f(h) append t)

  /**
    * Exercise 5.8
    *
    * This is more efficient than `cons(a, constant(a))` since it's just
    * one object referencing itself.
    *
    * def constant1[A](a: A): Stream[A] = Stream.cons(a, constant1(a))
    */
  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  /**
    * Exercise 5.9
    * Generates an infinite Stream of integers, starting from n, then n + 1, n + 2 and so on...
    */
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  /**
    * Exercise 5.10
    */
  def fibs: Stream[Int] = {
    //Auxiliary recursive method for generating a infinite Stream.
    def loop(f1: Int, f2: Int): Stream[Int] = {
      cons(f1, loop(f1, f1 + f2)) //Creates a Stream element with head = 'f1' and tail = 'loop(f1, f1 + f2)'
    }
    loop(0, 1) //Start values for loop
  }

  /**
    * Exercise 5.11
    *
    * The 'unfold function' takes an initial state 'z' and a function 'f' for producing
    * both the next state and the next value in the generated Stream.
    */
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((h, t)) => cons(h, unfold(t)(f))
      case None => empty
    }

  /**
    * Exercise 5.12
    *
    * Write fibs, from constant and ones in terms of unfold.
    *
    * Note: Functions with only one parameter can be written with '{ }'
    * with pattern matching anonymous function.
    */
  def fibsViaUnfold: Stream[Int] =
    unfold((0, 1))(s => s match {
      case (f1, f2) => Some((f1, (f1, f1 + f2)))
    }
      //Alt. syntax, pattern matching anonymous function:
      /*{
         case (f1, f2) => Some((f1, (f1, f1 + f2)))
       }*/
    )

  def fromViaUnfold(n: Int): Stream[Int] =
    unfold(n)(n => Some((n, n + 1)))

  def constantViaUnfold[A](a: A): Stream[A] =
    unfold(a)(_ => Some((a, a)))

  val onesViaUnfold: Stream[Int] =
    unfold(1)(_ => Some((1, 1)))


  /**
    * Exercise 5.12
    *
    * Use unfold to implement the functions map, take, takeWhile, zipWith and zipAll.
    */

  /**
    * mapViaUnfold:
    * Create an element in Stream[A].
    * If the Stream consist of a head h and tail t, process the element h by the function f,
    * and move on to the next element in the Stream by "running" the thunk t() on the tail.
    *
    * @param f function for transforming an Stream element of type A to a Stream element B
    * @tparam B type parameter.
    * @return a Stream of type B, Stream[B].
    */
  def mapViaUnfold[B](f: A => B): Stream[B] =
  //Handle 'this' stream. unfold handles initial state of type 'this', Stream[B].
    unfold(this) {
      //Create an element in Stream[A].
      //Process the head element and then process the tail element in the stream by t() thunk.
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    }

  /**
    * takeViaUnfold:
    * Create a new Stream[A] by taking n elements of initial 'this' Stream[A].
    *
    * @param n elements
    * @return a new Stream[A] of the initial Stream[A] 'this'.
    */
  def takeViaUnfold(n: Int): Stream[A] =
  //Handle 'this' stream and take n elements.
  //unfold handles initial state of type tuple2(Stream[B], n).
    unfold(this, n) {
      case (Cons(h, t), 1) => Some((h(), (empty, 0))) //Create Stream[B] by running thunk h(), and tuple2(empty,0)
      case (Cons(h, t), n) if n > 1 => Some((h(), (t(), n - 1))) //Process the head element and then process the tail element in the stream by t() thunk.
      case _ => None
    }

  /**
    * takeWhileViaUnfold:
    * Create a new Stream[A] as long as the predicate function 'p'
    * returns true from the initial 'this' Stream[A].
    *
    * @param p predicate function that each element in the 'this' Stream[A] must match against.
    * @return a new Stream[A], that contains elements that p(h()) is true.
    */
  def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if p(h()) => Some((h(), t()))
      case _ => None
    }

  /**
    * zipWith:
    * Combines two streams of type Stream[A] into one Stream[C].
    *
    * @param s2 the second Stream[B]
    * @param f  element combining function. Transform element type A and B into type C
    * @tparam B type parameter
    * @tparam C type parameter
    * @return a new Stream of type C.
    */
  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold(this, s2) {
      //Alt. pattern matching syntax s:(Stream[A], Stream[B]): (s => s match {...})
      //Combining thunks h1() and h2() into a new element in the newly zipped Stream[C].
      //Processing the next elements in the two streams by calling the tail thunks t1() and t2().
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
      case _ => None
    }

  /**
    * zipAll:
    *
    * @param s2
    * @tparam B
    * @return
    */
  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    zipWithAll(s2)((a, b) => (a, b))

  def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
    unfold((this, s2)) {
      case (Empty, Empty) => None //Both of the Streams are empty.
      //One Stream is not empty other is empty. Create a new Stream, by running thunk h() of Option, and running remaining Stream elements
      case (Cons(h, t), Empty) => Some(f(Some(h()), Option.empty[B]) ->(t(), empty[B]))
      case (Empty, Cons(h, t)) => Some(f(Option.empty[A], Some(h())) -> (empty[A] -> t()))
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())) -> (t1() -> t2()))
    }


  /**
    * Exercise 5.14
    *
    * startsWith checks if one Stream is a prefix of another.
    * E.g.: Stream(1,2,3) startsWith Stream(1,2) => true
    *
    * `s startsWith s2` when corresponding elements of `s` and `s2` are all equal, until the point that `s2` is exhausted.
    * If `s` is exhausted first, or we find an element that doesn't match, we terminate early.
    * Using non-strictness, we can compose these three separate logical steps--the zipping,
    * the termination when the second stream is exhausted, and the termination if a none-matching element is found or the first stream is exhausted.
    *
    */
  def startsWith[A](s: Stream[A]): Boolean =
  //First zip 'this' and s Stream, and ensure that we are zipping only equal elements number/where the s element is defined.
  //Then iterating over all elements by checking head h from first Stream is equal to h2 in second Stream.
  //So: zipping => termination when second stream is exhausted => termination of none-matching element is found in the first stream.
    zipAll(s).takeWhile(s => s._2.isDefined) forAll {
      case (h, h2) => h == h2
    }

  /**
    * Exercise 5.15
    *
    * For a given Stream, tails return the Stream of suffixes of the sequence, starting with the original Stream.
    * E.g.: Stream(1,2,3) => Stream(Stream(1,2,3), Stream(2,3), Stream(3), Stream()).
    * The last element of `tails` is always the empty `Stream`, so we handle this as a special case,
    * by appending it to the output.
    *
    */
  def tails: Stream[Stream[A]] =
    unfold(this) {
      //unfold's (f: S => Option[(A, S)]): Stream[A]:
      //should return an Option[Stream[A]], e.g. an Option(Tuple2(A, B))
      case Empty => None //Last element of Tails should be an empty Stream.
      case s: Stream[A] => Some((s, s drop 1))
    }

  /**
    * Exercise 5.16
    *
    * The function can't be implemented using `unfold`, since `unfold` generates elements of the `Stream` from left to right. It can be implemented using `foldRight` though.
    * The implementation is just a `foldRight` that keeps the accumulated value and the stream of intermediate results, which we `cons` onto during each iteration.
    * When writing folds, it's common to have more state in the fold than is needed to compute the result. Here, we simply extract the accumulated list once finished.
    */
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, p0) => {
      // p0 is passed by-name and used in by-name args in f and cons. So use lazy val to ensure only one evaluation...
      lazy val p1 = p0
      val b2 = f(a, p1._1)
      (b2, cons(b2, p1._2))
    })._2

}

case object Empty extends Stream[Nothing]

//Takes explicit "thunks": (() => A and () => Stream[A])
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  /**
    * A smart constructor for creating an nonempty stream
    *
    * @param hd the head of the stream
    * @param tl the tail of the stream
    * @tparam A type parameter
    * @return a new nonempty stream
    */
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    //We cache the head and tail as lazy to avoid repeated evaluation.
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  /**
    * A smart constructor for creating an empty stream of a particular type.
    *
    * @tparam A type parameter
    * @return a empty stream
    */
  def empty[A]: Stream[A] = Empty

  /**
    * A convenient variable-argument method for constructing a Stream from multiple elements.
    *
    * @param as elements
    * @tparam A type parameter
    * @return a Stream based on the input elements as: A*
    */
  def apply[A](as: A*): Stream[A] =
  // Makes use of smart constructors: as.head and apply(as.tail:_*) won't be evaluated
  // until we force the stream!!
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  /**
    * Infinite Streams
    */
  val ones: Stream[Int] = Stream.cons(1, ones)
}

/**
  * Testing the Stream functions.
  */
object StreamApp extends App {
  val si = Stream[Int]()
  val stream = Stream(1, 2, 3, 4, 5)
  //Calling foldRight with h and t thunks, the whole Stream will be processed.
  println(s"> foldRight - calling 'h' and 't' thunks: ${stream.foldRight(0)((h, t) => h + 100 + t)}")
  println(s"> foldRight - calling 'h' thunks: ${stream.foldRight(0)((h, t) => h + 100)}")
  println(s"> foldRight - calling 't' thunks: ${stream.foldRight(0)((h, t) => t + 100)}")
  println(s"> forAll - all elements in Stream is greater than 2: ${stream.forAll(h => h > 2)}")
  println(s"> drop: ${stream.drop(1).toList2}")
  println(s"> toList2: ${stream.toList2}")
  println(s"> exists2: ${stream.exists2(h => h == 2)}")
  println(s"> take(2).toList: ${stream.take(2).toList2}")
  println(s"> headOption2: ${stream.headOption2}")
  println(s"> map: ${stream.map(i => i + i).toList2}")
  println(s"> filter: ${stream.filter(i => i > 3).toList2}")
  println(s"> append: ${stream.append(Stream(88, 98, 78, 58)).toList2}")
  println(s"> flatMap: ${stream.flatMap(i => Stream(i * 10)).toList2}")
  println(s"> ones: ${ones.take(5).toList2}")
  println(s"> ones: ${ones.exists(_ % 2 != 0)}")
  println(s"> ones: ${ones.map(_ + 1).exists(_ % 2 == 0)}")
  println(s"> ones: ${ones.forAll(_ != 1)}")
}

























