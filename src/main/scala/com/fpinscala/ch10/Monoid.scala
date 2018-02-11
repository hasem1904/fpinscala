package com.fpinscala.ch10

import com.fpinscala.ch3.{Branch, Leaf, Tree}

/**
  * A Monoid trait:
  * - Laws of associative  and identity element.
  *
  * Created by havard on 6/18/17.
  */
trait Monoid[A] {
  /** Associative (e.g. binary) operation:
    * Satisfies:
    * op(op(x,y), z) == op(x, op(y, z))
    * */
  def op(a1: A, a2: A): A

  /** Identity for that operation (identity element => 'Neutral/Does nothing/zero').
    * Satisfies:
    * op(x, zero) == x and op(zero, x) == x
    * */
  def zero: A
}

/**
  * Concrete examples on the Monoid trait.
  */
object Monoid /*extends App*/ {

  /**
    * A String Monoid, that represents String concatenation.
    */
  val stringMonoid = new Monoid[String] {
    /** Associative operation for stringMonoid */
    override def op(a1: String, a2: String): String = a1 + a2

    /** Identity for stringMonoid. */
    val zero: String = ""
  }

  /**
    * List concatenation formed as a monoid
    *
    * @tparam A the type the list contains.
    * @return
    */
  def listMonoid[A] = new Monoid[List[A]] {
    /** Associative operation for listMonoid */
    override def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2

    /** Identity for listMonoid. */
    override def zero: List[A] = Nil
  }

  /** Exercise 10.1 */
  val intAddition: Monoid[Int] = new Monoid[Int] {
    /** Associative operation for intAddition */
    override def op(a1: Int, a2: Int): Int = a1 + a2

    /** Identity for intAddition. */
    override def zero: Int = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    /** Associative (e.g. binary) operation:
      * Satisfies:
      * op(op(x,y), z) == op(x, op(y, z))
      * */
    override def op(a1: Int, a2: Int) = a1 * a2

    /** Identity for that operation.
      * Satisfies:
      * op(x, zero) == x and op(zero, x) == x
      * */
    override def zero = 1
  }
  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    /** Associative (e.g. binary) operation:
      * Satisfies:
      * op(op(x,y), z) == op(x, op(y, z))
      * */
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2

    /** Identity for that operation.
      * Satisfies:
      * op(x, zero) == x and op(zero, x) == x
      * */
    override def zero: Boolean = true
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    /** Associative (e.g. binary) operation:
      * Satisfies:
      * op(op(x,y), z) == op(x, op(y, z))
      * */
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2

    /** Identity for that operation.
      * Satisfies:
      * op(x, zero) == x and op(zero, x) == x
      * */
    override def zero: Boolean = false
  }

  /** Exercise 10.2 */
  /*def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    /** Associative (e.g. binary) operation:
      * Satisfies:
      * op(op(x,y), z) == op(x, op(y, z))
      * */
    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2

    /** Identity for that operation (identity element => 'Neutral/Does nothing').
      * Satisfies:
      * op(x, zero) == x and op(zero, x) == x
      * */
    override def zero: Option[A] = None
  }*/

  // Notice that we have a choice in how we implement `op`.
  // We can compose the options in either order. Both of those implementations
  // satisfy the monoid laws, but they are not equivalent.
  // This is true in general--that is, every monoid has a _dual_ where the
  // `op` combines things in the opposite order. Monoids like `booleanOr` and
  // `intAddition` are equivalent to their duals because their `op` is commutative
  // as well as associative.
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(x: Option[A], y: Option[A]) = x orElse y

    val zero = None
  }

  // We can get the dual of any monoid just by flipping the `op`.
  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(x: A, y: A): A = m.op(y, x)

    val zero = m.zero
  }


  /** Exercise 10.3
    * Same argument and return type, called endMonoid
    * */
  def endMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    /** Associative (e.g. binary) operation:
      * Satisfies:
      * op(op(x,y), z) == op(x, op(y, z))
      * */
    def op(a1: A => A, a2: A => A): A => A = a1 compose a2

    /** Identity for that operation (identity element => 'Neutral/Does nothing').
      * Satisfies:
      * op(x, zero) == x and op(zero, x) == x
      * */
    val zero = (a: A) => a
  }

  /** Exercise 10.4 */
  //???


  /**
    * Folding List with Monoids:
    *
    * Associative:
    * def foldRight[B](z: B)(f: (A, B) => B) : B
    * def foldLeft[B](z: B)(f: (B, A) => B) : B
    *
    * What happens when A and B are of the same type:
    * def foldRight[A](z: A)(f: (A, A) => A): A
    * def foldLeft[A](z: A)(f: (A, A) => A): A
    */
  val words = List("Hic", "Est", "Index")

  /** Passing op and zero of stringMonoid i.o to reduce the list with the monoid and concatenate all strings in list */
  val foldRightStr = words.foldRight(stringMonoid.zero)(stringMonoid.op)
  val foldLeftStr = words.foldLeft(stringMonoid.zero)(stringMonoid.op)
  //foldRightStr and foldLeftStr will give the same result:
  println(s"> List concatenated with stringMonoid: $foldRightStr")
  println(s"> List concatenated with stringMonoid: $foldLeftStr")

  /** List concatenation with Monoid[A] */
  def concatenate[A](as: List[A], m: Monoid[A]): A = as.foldLeft(m.zero)(m.op)

  println(s"> concatenate: ${concatenate(words, stringMonoid)}")

  /** Exercise 10.5
    * When element type does not have a Monoid instance. Make use of map function
    * */
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  val as = List("2", 2)

  def funcTwo = (a: Any) => if (a == 2) " two" else a.toString

  println(s"> foldMap with words: ${foldMap(as, stringMonoid)(funcTwo)}")

  /** Exercise 10.6
    * def foldRight[B](z: B)(f: (A, B) => B): B
    * def foldLeft[B](z: B)(f: (B, A) => B): B
    * */
  // The function type `(A, B) => B`, when curried, is `A => (B => B)`.
  // And of course, `B => B` is a monoid for any `B` (via function composition
  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, endMonoid[B])(f.curried)(z)

  // Folding to the left is the same except we flip the arguments to
  // the function `f` to put the `B` on the correct side.
  // Then we have to also "flip" the monoid so that it operates from left to right.
  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as, endMonoid[B])(a => b => f(b, a))(z)

  /** Exercise 10.7
    * Implement foldMapV for IndexedSeq.
    * */
  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    if (as.isEmpty)
      m.zero
    else if (as.length == 1)
    //Running the only element through the Monoid function.
      f(as(0))
    else {
      val (l, r) = as.splitAt(as.length / 2)
      //Running the Monoid operation in parallel, recursively.
      println(s"> l: $l, r: $r")
      m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
    }

  /** Exercise 10.8
    * Implement parallel version of foldMap.
    * */

  /** Exercise 10.9
    * Implement parallel version of foldMap.
    * */


  /** Exercise 10.10
    * Implement parallel version of foldMap.
    * */
  sealed trait WC

  /** Stub is is the simplest case, where we haven't seen any complete words yet. */
  case class Stub(chars: String) extends WC

  /** Part keeps the number of complete words we've seen so far in words.
    * The value lStub holds any partial words we've seen so to teh left of those words, and rStub holds any
    * partial words on the right.
    * */
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    /** Associative (e.g. binary) operation:
      * Satisfies:
      * op(op(x,y), z) == op(x, op(y, z))
      * */
    //Sentence: "lorem ipsum dolor sit amet"
    override def op(a: WC, b: WC): WC = (a, b) match {
      case (Stub(c), Stub(d)) => Stub(c + d) //E.g. Stub("") + Stub("")
      case (Stub(c), Part(l, w, r)) => Part(c + l, w, r) //E.g.: Stub("") + Part("lorem", 1, "do")
      case (Part(l, w, r), Stub(c)) => Part(l, w, r + c) //E.g.: Part("lorem", 1, "do") + Stub("")
      case (Part(l1, w1, r1), Part(l2, w2, r2)) => //E.g.: Part("lorem", 1, "do") + Part("lor", 2 "")
        Part(l1, w1 + (if ((r1 + l2).isEmpty) 0 else 1) + w2, r2)
    }

    /** Identity for that operation (identity element => 'Neutral/Does nothing/zero').
      * Satisfies:
      * op(x, zero) == x and op(zero, x) == x
      * */
    override def zero: WC = Stub("")
  }

  /** Exercise 10.11
    * Implement parallel version of foldMap.
    * */
  def countWordsInString(str: String): Int = {
    //A single character's count. Whitespace does not count,
    //and non-whitespace starts a new Stub.
    def wc(c: Char): WC =
      if (c.isWhitespace) {
        println("|")
        Part("", 0, "")
      }
      else {
        //Debug
        println(s"> c: $c")
        Stub(c.toString)
      }

    //`unstub(s)` is 0 if `s` is empty, otherwise 1.
    def unstub(s: String) = {
      println(s"> unstub: $s, ${s.length}")
      s.length min 1
    }

    /**
      * foldMapV input parameters:
      * str.toIndexedSeq: "t, h, e,  , f, o, x,  , a, n, d,  , t, h, e,  , h, o, u, n, d"
      * wcMonoid: word count monoid for meeting the mon oid laws
      * wc: for each char in the IndexedSeq, the wc is run.
      */
    foldMapV(str.toIndexedSeq, wcMonoid)(wc) match {
      case Stub(s) => {
        //Debug
        println(s"> Stub(s): $s")
        unstub(s)
      }
      case Part(l, w, r) => {
        //Debug
        println(s"> unstub($l) + $w + unstub($r): ${unstub(l) + w + unstub(r)}")
        unstub(l) + w + unstub(r)
      }
    }
  }

  //Testing
  val wordStr = "the fox and the hound"
  //println(s"> words.toIndexedSeq: ${wordStr.toIndexedSeq}")
  val nwords = countWordsInString(wordStr)
  println(s"> Number of words in '$wordStr' is $nwords")

  /** Exercise 10.12
    * Implement Foldable[List], Foldable[IndexedSeq], Foldable[Stream],
    * */
  trait Foldable[F[_]] {
    def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
      foldMap(as)(f.curried)(endMonoid[B])(z)

    def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
      foldMap(as)(a => (b: B) => f(b, a))(dual(endMonoid[B]))(z)

    def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
      foldRight(as)(mb.zero)((a, b) => mb.op(f(a), b))

    def concatenate[A](as: F[A])(m: Monoid[A]): A =
      foldLeft(as)(m.zero)(m.op)

    /** Exercise 10.15
      * Implement Foldable[Tree]
      * */
    def toList[A](as: F[A]): List[A] =
      foldRight(as)(List[A]())(_ :: _)
  }

  /** A foldable List */
  object FoldableList extends Foldable[List] {
    override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
      as.foldRight(z)(f)

    override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
      as.foldLeft(z)(f)

    override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
      foldLeft(as)(mb.zero)((b, a) => mb.op(b, f(a)))
  }

  /** A foldable IndexedSeq */
  object FoldableIndexedSeq extends Foldable[IndexedSeq] {
    override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B =
      as.foldRight(z)(f)

    override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B =
      as.foldLeft(z)(f)

    override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
      foldMapV(as, mb)(f)
  }

  /** A foldable Stream */
  object FoldableStream extends Foldable[Stream] {
    override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B = /*super.foldRight(as)(z)(f)*/
      as.foldRight(z)(f)

    override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B = /*super.foldLeft(as)(z)(f)*/
      as.foldLeft(z)(f)
  }

  /** Exercise 10.13
    * Implement Foldable[Tree]
    * */
  /*
  sealed trait Tree[+A]
  case object Leaf[A](value:A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  */

  object FoldableTree extends Foldable[com.fpinscala.ch3.Tree] {
    override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B = as match {
      case Leaf(a) => f(z, a) //Start fold left
      case Branch(l, r) => foldLeft(r)(foldLeft(l)(z)(f))(f) // as -->|
    }

    override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B = as match {
      case Leaf(a) => f(a, z) //Start fold right
      case Branch(l, r) => foldRight(r)(foldRight(l)(z)(f))(f) // |<-- as
    }

    override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B = as match {
      case Leaf(a) => f(a) //Start fold left
      case Branch(l, r) => mb.op(foldMap(l)(f)(mb), foldMap(r)(f)(mb)) // splitting l and r tree in half recursively.
    }

    /** Exercise 10.14
      * Implement Foldable[Tree]
      * */
    object FoldableOption extends Foldable[Option] {
      override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B = as match {
        case None => z
        case Some(a) => f(z, a)
      }

      override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B = as match {
        case None => z
        case Some(a) => f(a, z)
      }

      override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B = as match {
        case None => mb.zero
        case Some(a) => f(a)
      }
    }

  }

  /** Exercise 10.16
    * Monoids can be composed.
    * E.g. that if types A and B are monoids, then the tuple type (A, B)
    * is also a monoid (called their product). Prove it by implementing
    * productMonoid.
    * */
  def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    /** Associative (e.g. binary) operation:
      * Satisfies:
      * op(op(x,y), z) == op(x, op(y, z))
      * */
    override def op(x: (A, B), y: (A, B)): (A, B) = (A.op(x._1, y._1), B.op(x._2, y._2))

    /** Identity for that operation (identity element => 'Neutral/Does nothing/zero').
      * Satisfies:
      * op(x, zero) == x and op(zero, x) == x
      * */
    override def zero: (A, B) = (A.zero, B.zero)
  }

  /**
    * Monoid for merging key-value Maps as long as the value type is monoid.
    **/
  def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] = new Monoid[Map[K, V]] {
    /**
      * Associative (e.g. binary) operation:
      * Satisfies:
      * op(op(x,y), z) == op(x, op(y, z))
      **/
    override def op(a: Map[K, V], b: Map[K, V]): Map[K, V] =
    //Combine the key set from a and b, starting with an empty Map.
      (a.keySet ++ b.keySet).foldLeft(zero) {
        //Accumulates with fold, getting hte accumulated value acc of type Map[K,V] (the new merged Map) and the next key k.
        (acc, k) =>
          //Add a key/value pair to this map, by using existing key k and new value from map value a and map value b.
          acc.updated(k, V.op(
            a.getOrElse(k, V.zero),
            b.getOrElse(k, V.zero)
          ))
      }

    /** Identity for that operation (identity element => 'Neutral/Does nothing/zero').
      * Satisfies:
      * op(x, zero) == x and op(zero, x) == x
      * */
    override def zero: Map[K, V] = Map[K, V]()
  }

  /**
    * Testing mapMergeMonoid
    */
  val M: Monoid[Map[String, Map[String, Int]]] = mapMergeMonoid(mapMergeMonoid(intAddition))
  val m1 = Map("01" -> Map("i1" -> 1, "i2" -> 2))
  val m2 = Map("01" -> Map("i2" -> 3))
  val m3 = M.op(m1, m2)
  println(s"> Testing mapMergeMonoid m3: ${m3.toString()}")

  /**
    * Exercise 10.17
    * Monoid instance for functions whose results are monoids.
    */
  def functionMonoid[A, B](B: Monoid[B]) = new Monoid[A => B] {
    /** Associative (e.g. binary) operation:
      * Satisfies:
      * op(op(x,y), z) == op(x, op(y, z))
      * */
    override def op(f: A => B, g: A => B): A => B = a => B.op(f(a), g(a))

    /** Identity for that operation (identity element => 'Neutral/Does nothing/zero').
      * Satisfies:
      * op(x, zero) == x and op(zero, x) == x
      * */
    override def zero: A => B = a => B.zero
  }

  /**
    * Exercise 10.18
    * Use monoids to compute a "bag" from an IndexedSeq
    */
  def bag[A](as : IndexedSeq[A]): Map[A, Int] =
  /**
    * foldMapV => for collection type IndexedSeq
    *
    * Monoid[B] => Using mapMergeMonoid with value monoid (V:Monoid[A]) to find the times the element appears in the bag.
    *              (Defining the associative op relation and identity element of the elements in the IndexedSeq).
    *
    * (f: A => B): B => Used for creating an associative Map, that maps the element in the IndexedSeq (as variable):
    *                   e.g: Map[A, Int] = Map(a -> 1)
    *
    * E.g.: bag(Vector("a", "rose","is","a", "rose"))
    *
    * (f: A => B): B = Map[String, Int]= Map(a -> 1, rose -> 1, is -> 1, a -> 1, rose -> 1)
    * The Monoid[B] = mapMergeMonoid[A, Int](intAddition) is used to combine the Map(a -> 1, rose -> 1, is -> 1, a -> 1, rose -> 1)
    * to get the result.
    */
    foldMapV(as, mapMergeMonoid[A, Int](intAddition))((a: A) => Map(a -> 1))

  //Testing
  val res = bag(Vector("a", "rose","is","a", "rose"))
  println(s"> bag = $res")


  /**
    * Using composed monoids to fuse traversals.
    */
  val mp = productMonoid(intAddition, intAddition)
  val p = FoldableList.foldMap(List(1,2,3,4))(a =>(1, a))(mp)
  println(s"> productMonoid: $p")
  println(s"> elements in list: ${p._1}")
  println(s"> Sum of list:      ${p._2}")
  val mean = p._2.doubleValue() / p._1
  println(s"> Mean: $mean")
}