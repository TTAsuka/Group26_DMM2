import play.api.libs.json._
import scala.io.Source
import scala.util.{Try, Success, Failure}
import java.time.ZonedDateTime
import java.time.format.{DateTimeFormatter, DateTimeParseException}

class EnergyController(filePath: String) {

  // Method to get the latest output power
  def getLatestOutput(): Try[Double] = Try {
    val source = Source.fromFile(filePath)
    try {
      val jsonString = source.mkString
      val json = Json.parse(jsonString)
      val data = (json \ "data").as[Seq[JsObject]]
      val latestEntry = data.maxBy { obj =>
        val startTimeStr = (obj \ "startTime").as[String]
        ZonedDateTime.parse(startTimeStr, DateTimeFormatter.ISO_DATE_TIME)
      }
      (latestEntry \ "value").as[Double]
    } catch {
      case e: Exception => throw new Exception("Failed to parse data: " + e.getMessage)
    } finally {
      source.close()
    }
  }

  // Method to display the latest output and optionally adjust it
  def displayAndOptionallyAdjustOutput(): Unit = {
    getLatestOutput() match {
      case Success(latestOutput) =>
        println(s"The current latest output power detected: $latestOutput kW")
        println("Do you want to operate to adjust the output power (y/n)?")
        val response = scala.io.StdIn.readLine().trim.toLowerCase

        response match {
          case "y" => adjustOutput()
          case "n" => println("No adjustments made.")
          case _   => println("Invalid input. Please enter 'y' or 'n'.")
        }

      case Failure(exception) =>
        println(s"Error: ${exception.getMessage}")
    }
  }

  // Method to adjust the output power
  private def adjustOutput(): Unit = {
    println("Please enter the new output power to adjust to (in kW):")
    Try(scala.io.StdIn.readDouble()) match {
      case Success(newOutput) =>
        println(s"Output will be adjusted to $newOutput kW.")
      case Failure(_) =>
        println("Invalid input. Please enter a valid double.")
    }
  }
}
