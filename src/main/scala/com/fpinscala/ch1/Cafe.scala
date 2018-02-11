package com.fpinscala.ch1

/**
  * Created by havard on 1/13/17.
  */
class Cafe {
  def buyCoffee(cc: CreditCard): (Coffee, Charge) = {
    val cup = new Coffee()
    //Returns a Tuple2
    (cup, Charge(cc, cup.price))
  }

  def buyCoffees(cc: CreditCard, n: Int): (List[Coffee], Charge) = {
    val purches: List[(Coffee, Charge)] = List.fill(n)(buyCoffee(cc))
    //unzip splits a list of pairs into a pair of list.
    //Here we're destructing this pair to declare two values (coffee and charges) on one line.
    val (coffees, charges) = purches.unzip

    //charges.reduce reduces the entire list of charges to a single charge, using the combine
    //to combine two at a time.
    (coffees, charges.reduce((c1, c2) => c1.combine(c2)))
  }
}

//Companion object
object Cafe {
  def main(args: Array[String]) {
    val cafe = new Cafe()
    val ccAnna = new CreditCard(123456, "Anna Peterson")
    val ccPeter = new CreditCard(127895, "Peter Peterson")

    val transAnna = cafe.buyCoffees(ccAnna, 10)
    val coffeesAnna = transAnna._1
    val chargeAnna = transAnna._2
    println("\nCoffee Transaction Anna:")
    coffeesAnna.foreach(println)
    println(chargeAnna)

    val transPeter = cafe.buyCoffees(ccPeter, 5)
    val chargePeter = transPeter._2
    val coffeesPeter = transPeter._1
    println("\nCoffee Transaction Peter:")
    coffeesPeter.foreach(println)
    println(chargePeter)

  }
}
