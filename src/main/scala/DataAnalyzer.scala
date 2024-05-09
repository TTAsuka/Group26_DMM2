import java.time.LocalDate
import java.time.LocalDateTime
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoField
import scala.io.Source
import play.api.libs.json._

import scala.util.{Failure, Try}

class DataAnalyzer {
  // DateTimeFormatter for parsing ISO 8601 formatted date-time strings
  private val dateTimeFormatter = DateTimeFormatter.ISO_ZONED_DATE_TIME
  // DateTimeFormatter for parsing ISO 8601 formatted date strings
  private val dateFormatter = DateTimeFormatter.ISO_LOCAL_DATE

  // Method to load data from a file
  def loadData(fileName: String): Seq[(LocalDateTime, Double)] = Try {
    val source = Source.fromFile(fileName)
    val jsonStr = try source.mkString finally source.close()
    Json.parse(jsonStr)
  }.flatMap { json =>
    Try((json \ "data").as[Seq[JsObject]].map { record =>
      // Extracting start time and value from JSON record
      val startTime = ZonedDateTime.parse((record \ "startTime").as[String], dateTimeFormatter).toLocalDateTime
      val value = (record \ "value").as[Double]
      (startTime, value)
    })
  }.recoverWith {
    case e: Exception =>
      // Error handling when loading data fails
      println(s"Error loading data from $fileName: ${e.getMessage}")
      Failure(e)
  }.getOrElse(Seq.empty)

  // Method to analyze data based on granularity, start, and end time
  def analyze(fileName: String, granularity: String, start: LocalDateTime, end: LocalDateTime): Unit = {
    println(s"Analyzing $granularity data from $start to $end")
    val records = loadData(fileName)
    val filteredRecords = filterByGranularity(records, granularity, start, end)
    if (filteredRecords.nonEmpty) {
      val values = filteredRecords.map(_._2)
      printStatistics(values)
    } else {
      println("No data available for analysis.")
    }
  }

  // Method to filter records by granularity, start, and end time
  private def filterByGranularity(records: Seq[(LocalDateTime, Double)], granularity: String, start: LocalDateTime, end: LocalDateTime): Seq[(LocalDateTime, Double)] = {
    records.filter { case (recordDate, _) =>
      granularity match {
        case "hourly" => recordDate.isAfter(start.minusMinutes(1)) && recordDate.isBefore(end.plusMinutes(1))
        case "daily" => recordDate.toLocalDate.isEqual(start.toLocalDate)
        case "weekly" => !recordDate.toLocalDate.isBefore(start.toLocalDate) && !recordDate.toLocalDate.isAfter(end.toLocalDate)
        case "monthly" => recordDate.getYear == start.getYear && recordDate.getMonth == start.getMonth
        case _ => false
      }
    }
  }

  // Method to print statistics such as mean, median, mode, range, and midrange of values
  private def printStatistics(values: Seq[Double]): Unit = {
    val mean = values.sum / values.size
    val sortedValues = values.sorted
    val median = if (values.size % 2 != 0) sortedValues(values.size / 2) else (sortedValues(values.size / 2 - 1) + sortedValues(values.size / 2)) / 2.0
    val mode = values.groupBy(identity).maxBy(_._2.size)._1
    val range = sortedValues.max - sortedValues.min
    val midRange = (sortedValues.max + sortedValues.min) / 2

    println(s"Mean: $mean")
    println(s"Median: $median")
    println(s"Mode: $mode")
    println(s"Range: $range")
    println(s"Midrange: $midRange")
  }
}
