package com.fpinscala.ch6

/**
  * Exercise 6.11: Candy Dispenser.
  *
  * Finite State Automaton that models a simple candy dispenser.
  * The machine has two types of input:
  * 1. You can insert a coin.
  * 2. Or you can turn a knob to dispense a candy.
  *
  * The candy dispenser can be in two states:
  * 1. Locked
  * 2. Unlocked
  *
  * The candy dispenser can also track how many candies are left and how many
  * coins it contains.
  *
  * Created by havard on 2/21/17.
  */
sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

import com.fpinscala.ch6.State._

object Candy {
  /**
    * The update state takes an input type (a Coin  or a Turn knob),
    * that result in a 'state machine action'. That 'state machine actions'
    * is then changes the state of the candy dispenser based on the following set of rules:
    *
    * ----------------------
    * Candy Dispenser rules.
    * ----------------------
    * 1. Inserting a coin into a locked machine will cause it to unlock if there's any candy left.
    *
    * 2. Turning the knob on a unlocked machine will cause it to dispense candy and become locked.
    *
    * 3. Turning the knob on a locked machine or inserting a coin into a unlocked machine, does nothing.
    *
    * 4. A machine that is out of candy ignores all inputs.
    *
    * @return
    */
  def update = (i: Input) => (s: Machine) => {
    //Handling the input type i (Coin/Turn) and the state of the machine:
    (i, s) match {
      //4. A machine that is out of candy ignores all inputs.
      case (_, Machine(_, 0, _)) => s
      //3. Turning the knob on a locked machine or inserting a coin into a unlocked machine, does nothing.
      case (Coin, Machine(false, _, _)) => s
      //3. Turning the knob on a locked machine or inserting a coin into a unlocked machine, does nothing.
      case (Turn, Machine(true, _, _)) => s
      //1. Inserting a coin into a locked machine will cause it to unlock if there's any candy left.
      case (Coin, Machine(true, candy, coin)) => Machine(locked = false, candy, coin + 1)
      //2. Turning the knob on a unlocked machine will cause it to dispense candy and become locked.
      case (Turn, Machine(false, candy, coin)) => Machine(locked = true, candy - 1, coin)
    }
  }

  /**
    * The SimulateMachine should operate the machine based on a list of inputs and
    * return the number of coins and candies left in the machine at the end.
    *
    * @param inputs
    * @return
    */

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
  //Loop through a list of Input types (Coins/Turn)
  /**
    * Rúnar's explanation:
    * Note the modify functions return type is State[S, Unit]. So the `map (modify[Machine] _ ...`, its an action that transitions
    * some state of type Machine and doesn't yield any return type. `modify` just transitions the state of type S.
    * It doesn't yield a value (hence the return type of Unit).
    * `inputs map (modify[Machine] _ compose update)`. This will give us a list of state actions (List[State[Machine, Unit]]).
    * Then we're calling sequence with those. That will create a single State action that runs all the actions in the list in sequence.
    * Now we have a `State[Machine, List[Unit]]`. A value of type List[Unit] isn't good for anything (except maybe getting its length),
    * so we discard that in the for-comprehension. What we really want is the end state of type Machine.
    * So we call get to retrieve the current Machine and assign that to s. Remember the type of get:
    * You should be able to work out how the for-comprehension desugars to map and flatMap calls:
    * It might help to read `map` and `flatMap` as "append" or "continue with".
    * You have some state-transitioning program, and `map` and `flatMap` append a further program at the end of it.
    *
    *
    * `sequence` takes a list of transitions and combines them into a single transition.
    * Each input type state (Coin/Turn) is then transformed by composing the `modify` and `update` function into on function.
    * Each input state (Coin/Turn) in the list, sets the state of the candy dispenser machine by `modify` function,
    * and then the `update` function is run on the candy dispenser machine (with the new state of the candy dispenser machine returned).
    * Then the final state (after running through the list of input types) of the candy dispenser machine, with the coins and candies is returned
    * as a tuple2.
    **/
    for {
      _ <- sequence(inputs map (modify[Machine] _ compose update))
      s <- get
    } yield (s.coins, s.candies)

}

//Running and testing the State machine...
object CandyDispenserMachineApp extends App {
  println("-----------------------------------")
  println("Setting up Candy Dispenser Machine:")
  println("-----------------------------------\n")
  val buyOneCandy = List[Input](Coin, Turn)

  //Buy one candy is two input state actions
  def buyCandies(times: Int): List[Input] = List.fill(times)(buyOneCandy).flatten

  println("-----------------------------------")
  println(s"> Candy Dispenser Machine Status: ")
  println("-----------------------------------")
  val machineInitStatus = Machine(locked = true, 5, 10)
  println(s"Locked:  ${machineInitStatus.locked}")
  println(s"Coins:   ${machineInitStatus.coins}")
  println(s"Candies: ${machineInitStatus.candies}\n")

  println("Buying four candies...")
  val cdmStatus = Candy.simulateMachine(buyCandies(4)).run(machineInitStatus)

  println("\n-----------------------------------")
  println(s"> Candy Dispenser Machine Status: ")
  println("-----------------------------------")
  println(s"Locked:  ${cdmStatus._2.locked}")
  println(s"Coins:   ${cdmStatus._2.coins}")
  println(s"Candies: ${cdmStatus._2.candies}")
  println("-----------------------------------")
  println("Coins and candies:")
  println(s"> Status coins: ${cdmStatus._1._1}")
  println(s"> Status candy: ${cdmStatus._1._2}")
  println("-----------------------------------")
}
