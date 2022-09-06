package fpfinal.model

import cats._
import cats.data.{NonEmptyChain, Validated}
import cats.data.Validated.{Invalid, Valid}
import cats.implicits._
import fpfinal.app.Configuration.IsValid
import fpfinal.common.Validations._
import fpfinal.model.Person.showPerson

/**
 * A class for representing a person.
 *
 * @param name the name of the person
 */
class Person private (val name: String) {
  /**
   * @return a string representation of this person which just shows the name
   */
  override def toString: String = showPerson.show(this)
}

object Person {
  /**
   * Creates an instance of Person without performing any validations.
   * Should only be used in tests.
   *
   * @param name the name of the person
   */
  def unsafeCreate(name: String): Person = new Person(name)

  /**
    * TODO #3d: Create a validated instance of Person. There are three validations:
    * - The name should not be empty
    * - The name should only contain letters
    * - The name should be at most 32 chars long
    */
  def create(name: String): IsValid[Person] = {
      //Accumulating errors in fast way
    (Validated.cond(name.nonEmpty, name, NonEmptyChain("The name can not be empty")),
      Validated.cond(containsNoSpecialChars(name), name, NonEmptyChain("The string shell not contain special characters")),
      Validated.cond(name.length < 32, name, NonEmptyChain("Name can not be longer than 32 characters")))
      .tupled
      .andThen(validatedName => Valid(Person.unsafeCreate(validatedName._1)))

  }



  def containsNoSpecialChars(string: String): Boolean = {
    val pattern = "^[a-zA-Z0-9]*$".r
    pattern.findAllIn(string).mkString.length == string.length
  }

  implicit val showPerson: Show[Person] = Show.show(_.name)

  implicit def eqPerson(implicit eqString: Eq[String]): Eq[Person] =
    Eq.instance((p1, p2) => p1.name === p2.name)

  implicit def ordPerson(implicit ordString: Order[String]): Order[Person] =
    Order.by(_.name)
}
