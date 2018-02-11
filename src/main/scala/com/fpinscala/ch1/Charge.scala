package com.fpinscala.ch1

/**
  * Created by havard on 1/13/17.
  */
case class Charge(cc: CreditCard, amount: Double) {
  def combine(other: Charge): Charge =
    if (cc == other.cc)
      Charge(cc, amount + other.amount)
    else
      throw new Exception("Can't combine charges to different cards!")

  /**
    * This function takes a list of charges, groups them by credit card used, and then
    * combines them into a single charge per card.
    *
    * Merge purges related to a specific credit card.
    * Passing functions as values to the groupBy, map and reduce methods.
    *
    * @param charges
    * @return
    */
  def coalesce(charges: List[Charge]): List[Charge] =
    //Grouping the Charges by cc and running over all entries by map
    //and combining each charge related to the cc.
    charges.groupBy(_.cc).values.map(_.reduce(_ combine _)).toList

  override def toString = s"Charge$cc, $amount)"
}
