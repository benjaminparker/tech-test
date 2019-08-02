import org.scalatest.{Matchers, WordSpec}
import java.time.LocalDate

import Functions._

class FunctionsTest extends WordSpec with Matchers {

  "sum" should {

    "return sum of given int list" in {
      val ints = List(1, 2, 3, 4, 5)
      sum(ints) shouldEqual 15
    }
  }

  "evenNumbers" should {

    "return list of even numbers given list of ints" in {
      val ints = List(1, 2, 3, 4, 5)
      evenNumbers(ints) shouldEqual List(2, 4)
    }
  }

  "dates" should {

    "return list of dates in string format yyyy-MM-dd from start point up to and including the range" in {
      val startDate = LocalDate.of(2019, 9, 1)
      dates(startDate, 2) should contain only("2019-09-01", "2019-09-02", "2019-09-03")
      dates(startDate, 0) should contain only "2019-09-01"
    }
  }

  val vehiclesUK = List(
    Vehicle(50, "Honda", 2, Some("AUTO")),
    Vehicle(20, "Toyota", 5, Some("MANUAL")),
    Vehicle(100, "BMW", 4)
  )

  val vehicleUS = List(
    Vehicle(60, "Merc", 2, Some("MANUAL")),
    Vehicle(40, "Fiat", 3, Some("MANUAL")),
    Vehicle(10, "Civic", 4, Some("AUTO")),
    Vehicle(0, "Punto", 0, Some("MANUAL"))
  )

  val vehiclesEU = List(
    Vehicle(70, "Jeep", 1, Some("AUTO")),
    Vehicle(25, "Fiat", 6),
    Vehicle(15, "Jaguar", 3),
    Vehicle(35, "Punto", 2)
  )

  // filter list of vehicles by the given sunroof type, possible values are AUTO, MANUAL or the option is None
  // if the car doesn't have a sunroof
  "vehiclesBySunroof" should {

    "Return list of vehicles filtered by given sunroof value" in {
      vehiclesBySunroof(vehiclesEU, "AUTO") should contain only Vehicle(70, "Jeep", 1, Some("AUTO"))

      vehiclesBySunroof(vehiclesEU ::: vehicleUS, "MANUAL") should contain only(
        Vehicle(0, "Punto", 0, Some("MANUAL")),
        Vehicle(40, "Fiat", 3, Some("MANUAL")),
        Vehicle(60, "Merc", 2, Some("MANUAL"))
      )
    }
  }

  // filter list of vehicles between the given speed boundaries, if the boundary is None ignore that limit
  "vehiclesBySpeed" should {

    "Return list of vehicles filtered with speed greater than given value" in {
      vehiclesBySpeed(vehiclesUK, Some(40), None) shouldEqual List(
        Vehicle(50, "Honda", 2, Some("AUTO")),
        Vehicle(100, "BMW", 4, None)
      )

      vehiclesBySpeed(vehicleUS ::: vehiclesUK, Some(55), None) shouldEqual List(
        Vehicle(60, "Merc", 2, Some("MANUAL")),
        Vehicle(100, "BMW", 4, None)
      )

      vehiclesBySpeed(vehiclesEU, None, None) shouldEqual vehiclesEU

      vehiclesBySpeed(vehiclesEU, Some(15), Some(70)) shouldEqual List(
        Vehicle(25, "Fiat", 6, None),
        Vehicle(35, "Punto", 2, None)
      )
    }
  }

  // return a list of the same vehicles with the model capitalised
  "vehiclesWithModelCapitalised" should {

    "return list of vehicles with their model capitalized" in {
      vehiclesWithModelCapitalised(vehiclesEU) shouldEqual List(
        Vehicle(70, "JEEP", 1, Some("AUTO")),
        Vehicle(25, "FIAT", 6, None),
        Vehicle(15, "JAGUAR", 3, None),
        Vehicle(35, "PUNTO", 2, None)
      )
    }
  }

  // Certain car manufacturers have asked for a specific message to be applied for their cars
  // Honda would like to display "Hello Honda driver, have a great day!" for their normal Honda drivers
  // Honda would like to display "Hello super fast Honda driver, zoom zoom!" for their fast(speed > 60) Honda drivers
  // Toyota would like to display "Morning Toyota fan, watch out you're going very fast!" for their fast(speed > 55) Toyota drivers
  // Toyota would like to display "Toyota driver! you're driving so fast you're losing wheels!" for their fast(speed > 70) Toyota drivers
  // who are losing wheels (numberOfWheels < 4)
  // Fiat would like to display "If it's a nice day, don't forget to roll down your fancy sunroof" for their Fiat drivers with
  // AUTO sunroofs
  // For anything not specified above display a simple "Have a good day driver"
  "personalisedMessage" should {

    "Return personalised message for standard Honda" in {
      personalisedMessageFor(Vehicle(60, "Honda", 5, Some("MANUAL"))) shouldEqual "Hello Honda driver, have a great day!"
    }

    "Return personalised message for fast Honda" in {
      personalisedMessageFor(Vehicle(61, "Honda", 5, Some("AUTO"))) shouldEqual "Hello super fast Honda driver, zoom zoom!"
    }

    "Return personalised message for fast Toyota" in {
      personalisedMessageFor(Vehicle(56, "Toyota", 5, Some("MANUAL"))) shouldEqual "Morning Toyota star, watch out you're going very fast!"
    }

    "Return personalised message for super-fast Toyota with less than 4 wheels" in {
      personalisedMessageFor(Vehicle(71, "Toyota", 3, Some("MANUAL"))) shouldEqual "Toyota driver! you're driving so fast you're losing wheels!"
    }

    "Return personalised message for fast Fiat with auto sunroof" in {
      personalisedMessageFor(Vehicle(71, "Fiat", 3, Some("AUTO"))) shouldEqual "If it's a nice day, don't forget to roll down your fancy sunroof"
    }

    "Return standard message for others" in {
      personalisedMessageFor(Vehicle(45, "Jaguar", 4, Some("MANUAL"))) shouldEqual "Have a good day driver"
    }
  }

  // return the total number of wheels by model for the given vehicle list
  // there is a bad implementation of this already, can you improve?
  "totalNumberWheelsByModel" should {

    "Return the total number of wheels for each model in the given list of vehicles" in {
      totalNumberWheelsByModel(vehiclesEU :+ Vehicle(40, "Punto", 3)) should contain only(
        ("Jeep", 1),
        ("Fiat", 6),
        ("Jaguar", 3),
        ("Punto", 5)
      )
    }
  }

  "modelDetailsFor" should {

    "Return details for given model" in {
      modelDetailsFor(Vehicle(40, "Punto", 3)) shouldBe ModelDetails(1, 0)
    }

    "Throw IllegalArgumentException for model not found" in {
      the[IllegalArgumentException] thrownBy {
        modelDetailsFor(Vehicle(40, "Ferrari", 3))
      } should have message "Cannot find details for Ferrari"
    }
  }

  "modelDetailsFor" should {

    "Return list of model details for given vehicles" in {
      modelDetailsFor(vehiclesEU) should contain only(
        ModelDetails(1, 0),
        ModelDetails(120, 2),
        ModelDetails(200, 8)
      )
    }
  }
}
