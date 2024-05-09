import java.time.LocalDateTime
import java.time.format.{DateTimeFormatter, DateTimeParseException}
import scala.io.StdIn._
import scala.collection.mutable
import play.api.libs.json._
import scala.io.Source
import scala.annotation.tailrec

class IssueDetector(energyType: String, filePath: String) {
  // Method to read a LocalDateTime from user input with error handling
  private def readDateTime(prompt: String): LocalDateTime = {
    val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm")

    @tailrec
    def attemptRead: LocalDateTime = {
      println(prompt)
      try {
        LocalDateTime.parse(readLine(), formatter)
      } catch {
        case e: DateTimeParseException =>
          println("Invalid format. Please use 'yyyy-MM-dd HH:mm'.")
          attemptRead
      }
    }

    attemptRead
  }

  // Method to load data from a JSON file
  private def loadData(): Seq[(LocalDateTime, Double)] = {
    val source = Source.fromFile(filePath)
    try {
      val jsonString = source.mkString
      val json = Json.parse(jsonString)

      (json \ "data").validate[Seq[JsObject]] match {
        case JsSuccess(dataSeq, _) => dataSeq.flatMap { data =>
          val startTimeStr = (data \ "startTime").as[String]
          val value = (data \ "value").as[Double]
          try {
            val startTime = LocalDateTime.parse(startTimeStr, DateTimeFormatter.ISO_DATE_TIME)
            Some((startTime, value))
          } catch {
            case e: DateTimeParseException =>
              println(s"Error parsing date time: $e")
              None
          }
        }
        case JsError(errors) =>
          println(s"Errors parsing JSON data: $errors")
          Seq.empty
      }
    } catch {
      case e: Exception =>
        println(s"Error reading or parsing JSON file: $e")
        Seq.empty
    } finally {
      source.close()
    }
  }

  // Energy thresholds for different types of energy sources
  private val energyThresholds = Map(
    "solar" -> 50.0,
    "hydro" -> 500.0,
    "wind" -> 500.0
  )

  // Method to detect and handle issues with energy data
  def detectAndHandleIssues(): Unit = {
    println("Detecting and handling issues...")

    // Prompting user for start and end times for issue detection
    val startTime = readDateTime("Enter the start time for issue detection (yyyy-MM-dd HH:mm):")
    val endTime = readDateTime("Enter the end time for issue detection (yyyy-MM-dd HH:mm):")

    // Loading data from file
    val data = loadData()
    val threshold = energyThresholds.getOrElse(energyType, 100.0)

    // Variables to track low energy counts and consecutive failures
    val (totalLowEnergyCount, lowEnergyCounts, consecutiveFailures) = data.foldLeft((0, Map.empty[LocalDateTime, Int], 0)) {
      case ((count, accum, consecutive), (time, value)) if time.isAfter(startTime) && time.isBefore(endTime) =>
        if (value < threshold) {
          val updatedCount = accum.getOrElse(time, 0) + 1
          val newConsecutive = consecutive + 1
          if (newConsecutive >= 10) {
            println(s"Warning: Potential equipment malfunction detected at $time. Stopping further checks.")
            return
          }
          (count + 1, accum.updated(time, updatedCount), newConsecutive)
        } else {
          (count, accum, 0)
        }
      case (acc, _) => acc
    }

    // Printing detected low energy instances and generating alerts
    println(s"Detected $totalLowEnergyCount instances of low energy output below the threshold of $threshold.")
    println("Generating alerts for the operators accordingly:")
    lowEnergyCounts.foreach { case (time, count) =>
      println(s"At $time, low energy output detected $count times.")
    }
  }
}
