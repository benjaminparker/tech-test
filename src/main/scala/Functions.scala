import java.time.LocalDate
import java.time.format.DateTimeFormatter

object Functions extends Enumeration {

  final val DateFormat = DateTimeFormatter.ofPattern("yyyy-MM-dd")

  //TODO Need to have a capitalisation free solution.  Perhaps Models should be case objects and not Strings
  case class Vehicle(speed: Int, model: String, numberOfWheels: Int, sunroofType: Option[String] = None)

  case class ModelDetails(size: Int, doors: Int)

  // Do not extend, we only have details for some models
  private final val ModelToDetails: Map[String, ModelDetails] = Map(
    "HONDA" -> ModelDetails(100, 4),
    "TOYOTA" -> ModelDetails(80, 6),
    "FIAT" -> ModelDetails(120, 2),
    "JEEP" -> ModelDetails(200, 8),
    "PUNTO" -> ModelDetails(1, 0)
  )

  def sum(ints: Seq[Int]): Int = ints.sum

  def evenNumbers(ints: Seq[Int]): Seq[Int] = {
    def isEven: Int => Boolean = _ % 2 == 0
    ints.filter(isEven)
  }

  def dates(startDate: LocalDate, noOfDays: Int): Seq[String] =
    (0 to noOfDays).map(startDate.plusDays(_).format(DateFormat))

  def vehiclesBySunroof(inputVehicles: Seq[Vehicle], sunroofType: String): Seq[Vehicle] =
    inputVehicles.filter(_.sunroofType contains sunroofType)

  def vehiclesBySpeed(inputVehicles: Seq[Vehicle], min: Option[Int], max: Option[Int]): Seq[Vehicle] =
    inputVehicles.filter(v => min.fold(true)(_ < v.speed) && max.fold(true)(_ > v.speed))

  def vehiclesWithModelCapitalised(inputVehicles: List[Vehicle]): List[Vehicle] = inputVehicles.map {
    v => v.copy(model = v.model.toUpperCase)
  }

  //TODO Need more messages - the general approach is shown
  def personalisedMessageFor(vehicle: Vehicle): String = vehicle match {
    case Vehicle(speed, model, _, _) if model.toUpperCase == "TOYOTA" && speed > 55 => "Morning Toyota star, watch out you're going very fast!"
    case _ => "Have a good day driver"
  }

  def totalNumberWheelsByModel(inputVehicles: Seq[Vehicle]): Seq[(String, Int)] =
    inputVehicles
      .groupBy(_.model)
      .mapValues(_.map(_.numberOfWheels).sum)
      .toSeq

  def modelDetailsFor(inputVehicle: Vehicle): ModelDetails =
    ModelToDetails.getOrElse(inputVehicle.model.toUpperCase, throw new IllegalArgumentException(s"Cannot find details for ${inputVehicle.model}"))

  def modelDetailsFor(inputVehicles: Seq[Vehicle]): Seq[ModelDetails] = inputVehicles.flatMap {
    vehicle => ModelToDetails.get(vehicle.model.toUpperCase)
  }
}
