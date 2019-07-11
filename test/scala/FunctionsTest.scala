import org.scalatest.{Matchers, WordSpec}
import java.time.LocalDate

import Functions._

class FunctionsTest extends WordSpec with Matchers {

  // add values in list together and return
  "sum" should {
    "Return sum of given int list" in {
      val ints = List(1,2,3,4,5)

      assert(sum(ints) == 15)
    }
  }

  // filter list to only even numbers
  "onlyEvenNumbers" should {
    "Return list of even numbers given list of ints" in {
      val ints = List(1,2,3,4,5)

      assert(onlyEvenNumbers(ints) == List(2,4))
    }
  }

  // generate a list of dates starting at the startdate and continuing for the given range, return as Strings
  "generateListOfDates" should {
    "Return list of dates in string format YYYY-MM-DD from start point up to and including the range" in {
      val startDate = LocalDate.of(1992, 9, 1)

      assert(generateListOfDates(startDate, 2) == List("2019-09-01", "2019-09-02", "2019-09-02"))
      assert(generateListOfDates(startDate, 0) == List("2019-09-01"))
    }
  }

  val vehiclesUK = List(Vehicle(50, "Honda", 2, Some("AUTO")), Vehicle(20, "Toyota", 5, Some("MANUAL")),
                        Vehicle(100, "BMW", 4, None))
  val vehicleUS = List(Vehicle(60, "Merc", 2, Some("MANUAL")), Vehicle(40, "Fiat", 3, Some("MANUAL")),
                       Vehicle(10, "Civic", 4, Some("AUTO")), Vehicle(0, "Punto", 0, Some("MANUAL")))
  val vehiclesEU = List(Vehicle(70, "Jeep", 1, Some("AUTO")), Vehicle(25, "Fiat", 6, None),
                        Vehicle(15, "Jaguar", 3, None), Vehicle(35, "Punto", 2, None))

  // filter list of vehicles by the given sunroof type, possible values are AUTO, MANUAL or the option is None
  // if the car doesn't have a sunroof
  "filterVehiclesBySunroof" should {
    "Return list of vehicles filtered by given sunroof value" in {
      assert(filterVehiclesBySunroof(vehiclesEU, "AUTO") == List(Vehicle(70, "Jeep", 1, Some("AUTO"))))
      assert(filterVehiclesBySunroof(vehiclesEU ::: vehicleUS, "MANUAL")
                                     == List(Vehicle(0, "Punto", 0, Some("MANUAL")), Vehicle(40, "Fiat", 3, Some("MANUAL")),
                                             Vehicle(60, "Merc", 2, Some("MANUAL"))))
    }
  }

  // filter list of vehicles between the given speed boundaries, if the boundary is None ignore that limit
  "filterVehiclesBySpeed" should {
    "Return list of vehicles filtered with speed greater than given value" in {
      assert(filterVehiclesBySpeed(vehiclesUK, Some(40), None) == List(Vehicle(100, "BMW", 4, None)))
      assert(filterVehiclesBySpeed(vehicleUS ::: vehiclesUK, Some(55), None)
                                   == List(Vehicle(60, "Merc", 2, Some("MANUAL")), Vehicle(70, "Jeep", 1, Some("AUTO"))))
      assert(filterVehiclesBySpeed(vehiclesEU, None, None) == vehiclesEU)
      assert(filterVehiclesBySpeed(vehiclesEU, Some(15), Some(70))
                                   == List(Vehicle(25, "Fiat", 6, None), Vehicle(35, "Punto", 2, None)))
    }
  }

  // return a list of the same vehicles with the model capitalised
  "generateVehiclesCapitalised" should {
    "Return list of vehicles filtered with speed greater than given value" in {
      assert(generateVehiclesCapitalised(vehiclesEU)
             == List(Vehicle(70, "JEEP", 1, Some("AUTO")), Vehicle(25, "FIAT", 6, None),
                     Vehicle(15, "JAGUAR", 3, None), Vehicle(35, "PUNTO", 2, None)))
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
  // Add in some more test cases if you can!
  "personalisedMessage" should {
    "Return personalised message given a vehicle" in {
      assert(personalisedMessage(Vehicle(56, "Toyota", 5, Some("MANUAL"))) == "Morning Toyota star, watch out you're going very fast!")
    }
  }

  // return the total number of wheels by model for the given vehicle list
  // there is a bad implementation of this already, can you improve?
  "totalNumberOfWheels" should {
    "Return the total number of wheels for each model in the given list of vehicles" in {
      assert(totalNumberWheelsByModel(vehiclesEU :+ Vehicle(40, "Punto", 3, None))
                                      == List(("Jeep", 1), ("Fiat", 6), ("Jaguar", 3), ("Punto", 5)))
    }
  }

  "getSingleVehicleModelDetails" should {
    "Return details for given model" in {
      assert(getSingleVehicleModelDetails(Vehicle(40, "Punto", 3, None)) == ModelDetails(1, 0))
    }
    "Throw correct exception for model not found" in {
      val caught = intercept[Exception](getSingleVehicleModelDetails(Vehicle(40, "Ferrari", 3, None)))
      assert(caught.getMessage == "Cannot find details for Ferrari")
    }
  }

  "getVehicleListModelDetails" should {
    "Return list of details for given vehicles" in {
      assert(getVehicleListModelDetails(vehiclesEU) == List(ModelDetails(1, 0)))
    }
  }

}
