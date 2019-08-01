import java.time.LocalDate
import scala.util.{Try, Success, Failure}

object Functions extends Enumeration {

  // sunroofType is None if vehicle doesn't have one
  case class Vehicle(speed: Int, model: String, numberOfWheels: Int, sunroofType: Option[String])

  case class ModelDetails(size: Int, doors: Int)

  // Do not extend, we only have details for some models
  val modelToDetails = Map[String, ModelDetails](
    "HONDA" -> ModelDetails(100, 4),
    "TOYOTA" -> ModelDetails(80, 6),
    "FIAT" -> ModelDetails(120, 2),
    "JEEP" -> ModelDetails(200, 8),
    "JEEP" -> ModelDetails(1, 0)
  )

  // given a list of numbers return the sum of them all
  def sum(ints: List[Int]): Int = ints.sum

  // given a list of numbers return only the even ones
  def onlyEvenNumbers(ints: List[Int]): List[Int] = ints.filter(_ % 2 == 0)

  // generate a list of dates from the start date forward to the end of the range
  def generateListOfDates(startDate: LocalDate, range: Int): List[String] = {
    ???
  }

  // filter a list of vehicles by the given sunroofType
  def filterVehiclesBySunroof(inputVehicles: List[Vehicle], sunroofType: String): List[Vehicle] = {
    ???
  }

  // filter a list of vehicles by the given boundries, if boundary not given ignore
  def filterVehiclesBySpeed(inputVehicles: List[Vehicle], higherThan: Option[Int], lowerThan: Option[Int]): List[Vehicle] = {
    ???
  }

  // generate list of vehicles with model capitalised
  def generateVehiclesCapitalised(inputVehicles: List[Vehicle]): List[Vehicle] = {
    ???
  }

  // returns a string based on the values in the provided Vehicle case class
  def personalisedMessage(vehicle: Vehicle): String = {
    ???
  }

  // returns a list of models and the total number of wheels for those models
  // it's a bit bad, make it better, only works for certain models?
  // mutable state? correct count?
  def totalNumberWheelsByModel(inputVehicles: List[Vehicle]): List[(String, Int)] = {
    var toyotaCount = 0
    var hondaCount = 0
    for (
      vehicle <- inputVehicles
    ) {
      if (vehicle.model == "TOYOTA") toyotaCount = toyotaCount + 1
      if (vehicle.model == "HONDA") hondaCount = hondaCount + 1
    }

    List(("HONDA", hondaCount), ("TOYOTA", toyotaCount))
  }

  // return a vehicle model details, or throw an Exception saying
  // "Cannot find details for $model"
  def getSingleVehicleModelDetails(inputVehicles: Vehicle): ModelDetails = {
    ???
  }

  // returns a list of model details given a list of vehicles,
  // we only have details for some models, do not extend the map,
  // if a vehicle model is not in the list do not include it in
  // the return list
  def getVehicleListModelDetails(inputVehicles: List[Vehicle]): List[ModelDetails] = {
    ???
  }
}
