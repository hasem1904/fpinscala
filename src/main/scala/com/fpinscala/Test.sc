
val list = List(1, 2, 3, 4, 5)

for (
  x <- list
) yield x + 12

val res3 = for (al <- list if al > 3) yield al + 1
val res4 = list.filter(_ > 3).map(_ + 1)

case class Person(firstname: String, lastname: String, occupation: String)

val p1 = Person("Peter", "Warren", "Investor")
val p2 = Person("Warren", "Buffet", "Investor")
val p3 = Person("George", "Soros", "Investor")
val p4 = Person("Jennifer", "Lopez", "Actress")
val p5 = Person("Bill", "Gates", "Entrepreneur")
val p6 = Person("Larry", "Eliason", "Entrepreneur")

val persons = List(p1, p2, p3, p4)

val res =
  for (
    p <- persons
    if "Investor".equals(p.occupation)
  ) yield p

val investorsEntrepreneurs = p5 :: p6 :: res
for (ie <- investorsEntrepreneurs)
  println(ie)

import scala.collection.mutable.ArrayBuffer

//Mutable ArrayBuffer
var peoples = ArrayBuffer[Person]()
peoples = peoples += p1
peoples = peoples += p2
peoples = peoples += p3
peoples = peoples += p4
peoples = peoples += p5
peoples = peoples += p6

println(s"> peoples size: ${peoples.size}")
peoples.foreach(p => println(p))