package com.fpinscala.ch14

//import fpinscala.monads._

object Mutable {
  def quicksort(xs: List[Int]): List[Int] = if (xs.isEmpty) xs else {
    val arr = xs.toArray

    def swap(x: Int, y: Int) = {
      val tmp = arr(x)
      arr(x) = arr(y)
      arr(y) = tmp
    }

    def partition(l: Int, r: Int, pivot: Int) = {
      val pivotVal = arr(pivot)
      swap(pivot, r)
      var j = l
      for (i <- l until r) if (arr(i) < pivotVal) {
        swap(i, j)
        j += 1
      }
      swap(j, r)
      j
    }

    def qs(l: Int, r: Int): Unit = if (l < r) {
      val pi = partition(l, r, l + (r - l) / 2)
      qs(l, pi - 1)
      qs(pi + 1, r)
    }

    qs(0, arr.length - 1)
    arr.toList
  }
}

/**
  * The new local-effects monad ST trait, which could stand for:
  * - state thread
  * - state transmission
  * - state token
  * - state tag
  *
  * @tparam S
  * @tparam A
  */
sealed trait ST[S, A] {
  self =>
  protected def run(s: S): (A, S)

  def map[B](f: A => B): ST[S, B] = new ST[S, B] {
    def run(s: S) = {
      val (a, s1) = self.run(s)
      (f(a), s1)
    }
  }

  def flatMap[B](f: A => ST[S, B]): ST[S, B] = new ST[S, B] {
    def run(s: S) = {
      val (a, s1) = self.run(s)
      f(a).run(s1)
    }
  }
}

/**
  * ST action companion object.
  */
object ST {
  def apply[S, A](a: => A) = {
    lazy val memo = a
    new ST[S, A] {
      def run(s: S) = (memo, s)
    }
  }

  def runST[A](st: RunnableST[A]): A =
    st[Null].run(null)._1
}

/**
  * The STRef is a data structure used for mutable references, is a wrapper around a protected var.
  *
  * @tparam S
  * @tparam A
  */
sealed trait STRef[S, A] {
  protected var cell: A //Protected var
  def read: ST[S, A] = ST(cell) //pure function, returning a ST action.
  def write(a: => A): ST[S, Unit] = new ST[S, Unit] {
    def run(s: S) = {
      cell = a
      ((), s) //pure function, returning a ST action.
    }
  }
}

/**
  * The STRef companion object, only way to constructing an instance of STRef, is through the apply method.
  */
object STRef {
  def apply[S, A](a: A): ST[S, STRef[S, A]] = ST(new STRef[S, A] {
    var cell = a
  })
}

/**
  * ST actions, actions that are safe to run, actions that are polymorphic in S.
  *
  * @tparam A
  */
trait RunnableST[A] {
  def apply[S]: ST[S, A]
}

// Scala requires an implicit Manifest for constructing arrays.
sealed abstract class STArray[S, A](implicit manifest: Manifest[A]) {
  //Mutable Array containers
  protected def value: Array[A]

  def size: ST[S, Int] = ST(value.size)

  // Write a value at the give index of the array
  def write(i: Int, a: A): ST[S, Unit] = new ST[S, Unit] {
    def run(s: S) = {
      value(i) = a
      ((), s)
    }
  }

  // Read the value at the given index of the array
  def read(i: Int): ST[S, A] = ST(value(i))

  // Turn the array into an immutable list
  def freeze: ST[S, List[A]] = ST(value.toList)

  /**
    * Exercise 14.1:
    * Fill combinator.
    * For example, xs.fill(Map(0 -> "a", 2 -> "b")) should write the
    * value "a" at index 0 in the array xs and "b" at index 2.
    * Using the ST Monad.
    *
    * @param xs
    * @return
    */
  def fill(xs: Map[Int, A]): ST[S, Unit] =
  //Starting with an empty ST Monad...
    xs.foldRight(ST[S, Unit](())) {
      /**
        * Case when a key k, value v and a given state st:
        * flatMap the state ST monad, and write the value at the given index of the array
        * with the ST Monad write method.
        */
      case ((k, v), st) => st flatMap (_ => write(k, v))
    }

  /** A swap function that swaps two elements of the array. */
  def swap(i: Int, j: Int): ST[S, Unit] = for {
    x <- read(i)
    y <- read(j)
    _ <- write(i, y)
    _ <- write(j, x)
  } yield ()
}

/** Companion object for creating a mutable Array. */
object STArray {
  /** Construct an array of the given size filled with the value v */
  def apply[S, A: Manifest](sz: Int, v: A): ST[S, STArray[S, A]] =
    ST(new STArray[S, A] {
      lazy val value = Array.fill(sz)(v)
    })

  /**
    * The fromList primitive does the following:
    * Turning a list into an array
    **/
  def fromList[S, A: Manifest](xs: List[A]): ST[S, STArray[S, A]] =
    ST(new STArray[S, A] {
      lazy val value = xs.toArray
    })
}

object Immutable {
  //An action that does nothing
  def noop[S] = ST[S, Unit](())

  /**
    * Exercise 14.2:
    * The partition method partitions a portion of the array into elements less than
    * and greater than a pivot.
    */
 /* def partition[S](a: STArray[S, Int], l: Int, r: Int, pivot: Int): ST[S, Int] = for {
    vp <- a.read(pivot) //read the pivotValue
    _ <- a.swap(pivot, r) //swaps to elements in the array
    j <- STRef(l) //STRef, mutable state
    _ <- (l until r).foldLeft(noop[S])((s, i) => for { //Folding over from left to right, starting with a zero action, and wit a state s and index i.
      _ <- s
      vi <- a.read(i)
      //Sort/swap left to pivot part of array...
      _ <- if (vi < vp) (for { //When the value vi is less than the pivot value.
        vj <- j.read //Read the STRef mutable state value j.
        _ <- a.swap(i, vj) //Swap the elements in the array, value i and value j from STRef .
        _ <- j.write(vj + 1) //Handle next value.
      } yield ()) else noop[S] //Return a action that does nothing (array is empty or finished sorting...)
    } yield ()) //Sort/swap right of the pivot part of the array.
    x <- j.read //Read STRef value
    _ <- a.swap(x, r) //Swap the values to the right elements in the array of the pivot.
  } yield x //return x /pivot value.*/

  /**
    * Exercise 14.2:
    * The qs method sorts a portion of the array in place.
    */
 /* def qs[S](a: STArray[S, Int], l: Int, r: Int): ST[S, Unit] = if (l < r) for {
    pi: Int <- partition(a, l, r, l + (r - l) / 2) //Returns a partition index of the array into elements less than and greater than a pivot.
    _ <- qs(a, l, pi - 1) //Recursivly sort the left part of the array.
    _ <- qs(a, pi + 1, r) //Recursivly sort the right part of the array.
  } yield () else noop[S] //Returns a ST(S, Unit) action or an action that does nothing.*/

  /** quicksort with the ST monad */
  /*def quicksort(xs: List[Int]): List[Int] =
    if (xs.isEmpty) xs else ST.runST(new RunnableST[List[Int]] {
      def apply[S] = for {
        arr <- STArray.fromList(xs)
        size <- arr.size
        _ <- qs(arr, 0, size - 1)
        sorted <- arr.freeze
      } yield sorted
    })*/
}

/**
  * Exercise 14.3:
  * Minimal set of primitives combinators
  * for creating and manipulating hash maps.
  */
import scala.collection.mutable.HashMap

sealed trait STMap[S,K,V] {
  //A mutable HashMap table.
  protected def table: HashMap[K,V]

  //Get the size of the HashMap
  def size: ST[S,Int] = ST(table.size)

  //Get the value under a key
  def apply(k: K): ST[S,V] = ST(table(k))

  //Get the value under a key, or None if the key does not exist
  def get(k: K): ST[S, Option[V]] = ST(table.get(k))

  //Add a value under a key
  def +=(kv: (K, V)): ST[S,Unit] = ST(table += kv)

  //Remove a key
  def -=(k: K): ST[S,Unit] = ST(table -= k)
}

/** Companion object for creating a mutable HashTable. */
object STMap {
  /** Construct an empty hash table */
  def empty[S,K,V]: ST[S, STMap[S,K,V]] = ST(new STMap[S,K,V] {
    val table = HashMap.empty[K,V]
  })

  /**
    * The fromMap primitive does the following:
    * Turning a map into a HashMap
    **/
  def fromMap[S,K,V](m: Map[K,V]): ST[S, STMap[S,K,V]] = ST(new STMap[S,K,V] {
    val table = (HashMap.newBuilder[K,V] ++= m).result
  })
}


object LocalEffectsApp extends App {
  val res: ST[Nothing, (Int, Int)] = for {
    r1 <- STRef[Nothing, Int](1)
    r2 <- STRef[Nothing, Int](1)
    x <- r1.read
    y <- r2.read
    _ <- r1.write(y + 1)
    _ <- r2.write(x + 1)
    a <- r1.read
    b <- r2.read
  } yield (a, b)
  println(res)

  val p = new RunnableST[(Int, Int)] {
    override def apply[S] =
      for {
        r1 <- STRef(1)
        r2 <- STRef(2)
        x <- r1.read
        y <- r2.read
        _ <- r1.write(y + 1)
        _ <- r2.write(x + 1)
        a <- r1.read
        b <- r2.read
      } yield (a, b)
  }
  val r = ST.runST(p)
  println(s"> Results: $r")

}



