package com.fpinscala.ch3

/**
  * Created by havard on 1/21/17.
  */

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  //Exercise: 3.25
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + (size(l) + size(r))
  }

  //Exercise: 3.26
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  //Exercise: 3.27
  def depth(t: Tree[Int]): Int = t match {
    case Leaf(v) => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  //Exercise: 3.28
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  //Exercise: 3.29: Function f handles value in one subtree tree. Function g handles both subtrees.
  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(v) => f(v)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g)) //Handle left and right tree.
  }

  def sizeViaFold[A](t: Tree[A]): Int = fold(t)(i => 1)((b1, b2) => 1 + b1 + b2)
  def maximumViaFold(t: Tree[Int]): Int = fold(t)(i => i)((b1, b2) => b1 max b2)
  def depthViaFold(t: Tree[Int]): Int = fold(t)(i => 0)((b1, b2) => 1 + (b1 max b2))
  def mapViaFold[A, B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(a => Leaf(f(a)):Tree[B])(Branch(_, _))
}

object TreeApp extends App{
    //Test data...
    val leafLeft = Leaf(1)
    val leafRight = Leaf(10)
    val branchLeft = Branch(leafLeft, leafRight)
    val branchRight = Branch(leafLeft, leafRight)
    val tree = Branch(branchLeft, branchRight)
    //Testing functions...
    println(s"size:           ${Tree.size(tree)}")
    println(s"sizeViaFold:    ${Tree.sizeViaFold(tree)}")
    println(s"maximum:        ${Tree.maximum(tree)}")
    println(s"maximumViaFold: ${Tree.maximumViaFold(tree)}")
    println(s"depth:          ${Tree.depth(tree)}")
    println(s"depthViaFold:   ${Tree.depthViaFold(tree)}")
    println(s"map:            ${Tree.map(tree)(a => a + 10)}")
    println(s"mapViaFold:     ${Tree.mapViaFold(tree)(a => a + 10)}")
    println(s"map:            ${Tree.map(tree)(a => a + " element")}")
    println(s"mapViaFold:     ${Tree.mapViaFold(tree)(a => a + " element")}")
}
