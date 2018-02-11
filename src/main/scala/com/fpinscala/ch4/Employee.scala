package com.fpinscala.ch4

/**
  * Created by havard on 1/23/17.
  */
case class Employee(name: String, department: String, manager: Option[Employee])

object Employee {
  //Management
  val ceoKatie = Employee("Kathie", "Management", None)
  val ctoAlbert = Employee("Albert", "Management", Some(ceoKatie))
  val cmoLaurene = Employee("Laurene", "Management", Some(ceoKatie))
  //Employees
  val peter = Employee("Peter", "Finance", Some(ceoKatie))
  val ellen = Employee("Ellen", "Finance", Some(ceoKatie))
  val andrea = Employee("Andrea", "Finance", Some(ceoKatie))
  val harold = Employee("Harold", "Marketing", Some(cmoLaurene))
  val steve = Employee("Steve", "Marketing", Some(cmoLaurene))
  val joe = Employee("Joe", "Marketing", Some(cmoLaurene))
  val andrew = Employee("Andrew", "IT", Some(ctoAlbert))

  val employeeDb = Seq[Employee](peter, ellen, andrea, harold, steve, joe, andrew)

  def lookupByName(name: String): Option[Employee] =
    if (name.isEmpty) None
    else Some(employeeDb.find(e => e.name == name).get)

  val DefaultDept: String = "Default Dept."

  /**
    * Application entry point.
    * Test of Option methods related to the Employee class.
    *
    * @param args
    */
  def main(args: Array[String]) {

    //Map: Joe's department or None if Joe is not an employee
    //val joeDepartmentViaMap: Option[String] = lookupByName("Joe").map(_.department)
    val joeDepartmentViaMap: Option[String] = lookupByName("Joe").map((e) => e.department) //Note!! (f: A => B), Employee => Employee.department:[String]
    println(s"Joe's Department (map) : $joeDepartmentViaMap")

    //FlatMap: Joe's department or None if Joe is not an employee
    //val joeManagerViaFlatMap = lookupByName("Joe").flatMap(_.manager)
    val joeManagerViaFlatMap = lookupByName("Joe").flatMap((e) => e.manager) //Note!! (f: A => Option[B]),  Employee => Employee.manager:Option[Employee]

    //val joeManagerViaFlatMap = lookupByName("Joe").flatMap ( _.department)
    println(s"Joe's Department (flatMap) : $joeManagerViaFlatMap")

    //FlatMap: Joe's department (as String) or None if Joe is not an employee
    val joeDepartmentViaGetOrElse = lookupByName("Joe").map(_.department).getOrElse(DefaultDept)
    println(s"Joe's Department (getOrElse) : $joeDepartmentViaGetOrElse")

    //A common pattern is to transform an Option via calls to map, flatMap and/or filter, and then use
    //getOrElse to do error handling at the end:
    //Note!! Here getOrElse is used to transform Option[String] to a String by providing a default department.
    val dept: String = lookupByName("Joe").map(_.department).filter(_ != "Accounting").getOrElse(DefaultDept)

  }
}
