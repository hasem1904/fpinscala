package com.fpinscala.ch1

/**
  * Created by havard on 1/13/17.
  */
case class CreditCard(ccNumber: Int, ccHolder: String) {
  override def toString = s"CreditCard($ccNumber, $ccHolder)"
}
