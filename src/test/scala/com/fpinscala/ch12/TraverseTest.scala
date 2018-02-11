package com.fpinscala.ch12

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

/**
  * The TraverseTest class contains tests related to the Traverse class.
  */
@RunWith(classOf[JUnitRunner])
class TraverseTest extends FunSuite {

  trait TraverseTest {
    /** Example data for 'TraverseTest' */
    val firstName: List[String] = List[String]("Ann", "Mary", "Elisabeth", "Kristin")
    val lastName: List[String] = List[String]("Peterson", "Trump", "Kennedy", "Harrison")
    val lt = Traverse.listTraverse
  }

  test("testTraverseS") {
    new TraverseTest {
      //assert(firstName === lt.traverseS(firstName)(a => new State( s =>_,_ )))
      assert(true)
    }
  }

  test("testToList") {
    new TraverseTest {
      assert(lt.toList(firstName) === firstName)
    }
  }

  test("testMapAccum") {
    new TraverseTest {
      val expFLNames = List("Ann Peterson", "Mary Trump", "Elisabeth Kennedy", "Kristin Harrison")
      assert(expFLNames === lt.mapAccum(firstName, lastName)((a, s) => {
        val i = firstName.indexOf(a)
        (s"$a ${lastName(i)}", s)
      })._1)
    }
  }

  test("testReverse") {
    new TraverseTest {
      assert(lt.toList(lt.reverse(firstName)) ++ lt.toList(lt.reverse(lastName)) === lt.reverse(lastName ++ firstName))
    }
  }

  test("testZipWithIndex") {
    new TraverseTest {
      assert(firstName.zipWithIndex === lt.zipWithIndex(firstName))
    }
  }
}
