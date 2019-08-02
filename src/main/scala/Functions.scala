import java.time.LocalDate
import java.time.format.DateTimeFormatter

object Functions extends Enumeration {

  final val DateFormat = DateTimeFormatter.ofPattern("yyyy-MM-dd")

  // sunroofType is None if vehicle doesn't have one
  case class Vehicle(speed: Int, model: String, numberOfWheels: Int, sunroofType: Option[String] = None)

  case class ModelDetails(size: Int, doors: Int)

  // Do not extend, we only have details for some models
  val modelToDetails = Map[String, ModelDetails](
    "HONDA" -> ModelDetails(100, 4),
    "TOYOTA" -> ModelDetails(80, 6),
    "FIAT" -> ModelDetails(120, 2),
    "JEEP" -> ModelDetails(200, 8),
    "PUNTO" -> ModelDetails(1, 0)
  )

  // given a list of numbers return the sum of them all
  def sum(ints: List[Int]): Int = ints.sum

  // given a list of numbers return only the even ones
  def onlyEvenNumbers(ints: List[Int]): List[Int] = ints.filter(_ % 2 == 0)

  // generate a list of dates from the start date forward to the end of the range
  def generateListOfDates(startDate: LocalDate, range: Int): Seq[String] = (0 to range) map {
    startDate.plusDays(_).format(DateFormat)
  }

  // filter a list of vehicles by the given sunroofType
  def filterVehiclesBySunroof(inputVehicles: List[Vehicle], sunroofType: String): List[Vehicle] = {
    inputVehicles.filter(_.sunroofType contains sunroofType)
  }

  // filter a list of vehicles by the given boundaries, if boundary not given ignore
  def filterVehiclesBySpeed(inputVehicles: List[Vehicle], min: Option[Int], max: Option[Int]): List[Vehicle] =
    inputVehicles.filter { vehicle =>
      (vehicle, min, max) match {
        case (v, Some(mn), Some(mx)) => mn < v.speed && v.speed < mx
        case (v, None, Some(mx)) => v.speed < mx
        case (v, Some(mn), None) => v.speed > mn
        case (_, None, None) => true
      }
    }

  // generate list of vehicles with model capitalised
  def generateVehiclesCapitalised(inputVehicles: List[Vehicle]): List[Vehicle] = inputVehicles.map { v =>
    v.copy(model = v.model.toUpperCase)
  }

  // returns a string based on the values in the provided Vehicle case class
  def personalisedMessage(vehicle: Vehicle): String = vehicle match {
    case Vehicle(speed, "Toyota", _, _) if speed > 55 => "Morning Toyota star, watch out you're going very fast!"
    case _ => "Have a good day driver"
  }

  // returns a list of models and the total number of wheels for those models
  // it's a bit bad, make it better, only works for certain models?
  // mutable state? correct count?
  def totalNumberWheelsByModel(inputVehicles: List[Vehicle]): List[(String, Int)] = {
    inputVehicles.groupBy(_.model).mapValues(_.map(_.numberOfWheels).sum)
  }.toList

  // return a vehicle model details, or throw an Exception saying
  // "Cannot find details for $model"
  def getSingleVehicleModelDetails(inputVehicle: Vehicle): ModelDetails = {
    modelToDetails.getOrElse(inputVehicle.model.toUpperCase,throw new IllegalArgumentException(s"Cannot find details for ${inputVehicle.model}"))
  }

  // returns a list of model details given a list of vehicles,
  // we only have details for some models, do not extend the map,
  // if a vehicle model is not in the list do not include it in
  // the return list
  def getVehicleListModelDetails(inputVehicles: List[Vehicle]): List[ModelDetails] = {
    inputVehicles flatMap { v=>
      modelToDetails.get(v.model.toUpperCase)
    }
  }
}