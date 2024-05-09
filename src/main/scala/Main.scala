import scala.io.StdIn._
import java.time.{LocalDateTime, YearMonth}
import java.time.format.DateTimeFormatter
import java.time.LocalDate
import java.time.format.{DateTimeFormatter, DateTimeParseException}
import scala.util.{Try, Success, Failure}
import scala.io.StdIn
import scala.annotation.tailrec
import java.time.{LocalDate, LocalDateTime, YearMonth}
import java.time.format.DateTimeFormatter

object Main extends App {
   // Display menu
  @tailrec
  def mainMenu(): Unit = {
    println("\n--- Welcome to Renewable Energy Plant System! ---")
    println("1. Fetch and store data related to energy generated from renewable sources")
    println("2. Monitor and Control Energy Sources")
    println("3. View Energy Generation and Storage Data")
    println("4. Analyze Energy Data")
    println("5. Detect and handle issues with renewable energy sources")
    println("6. Exit")

    println("Select an option: ")
    val choice = readLine()

    choice match {
      case "1" => fetchDataForEnergySources()
      case "2" => monitorAndControl()
      case "3" => viewEnergy()
      case "4" => analyzeData()
      case "5" => detectAndHandleIssues()
      case "6" =>
        println("Exiting system...")
        return
      case _ =>
        println("Invalid choice, please enter a number between 1 and 6.")
    }

    mainMenu()
  }

  mainMenu()

  // Function to fetch data for different energy sources within a specified time range
  def fetchDataForEnergySources(): Unit = {
    val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm")
    println("Please enter the start time and end time of the data you want to obtain!")

    // Recursively read and validate start time
    val startTime = readDateTime("Enter the Start time (format yyyy-MM-dd HH:mm)(1-2 months of data is recommended due to api limitations):", formatter)

    // Recursively read and validate end time
    val endTime = readDateTime("Enter the End time (format yyyy-MM-dd HH:mm):", formatter)

    val isoFormatter = DateTimeFormatter.ISO_DATE_TIME
    val formattedStartTime = startTime.format(isoFormatter) + "Z"
    val formattedEndTime = endTime.format(isoFormatter) + "Z"

    val api = new FetchAndSaveAPIData("https://data.fingrid.fi/api/data", "60ba3ea704b241f18e4ee3813423cdd4")
    val datasets = Map(248 -> "solar_data.json", 191 -> "hydro_data.json", 75 -> "wind_data.json")

    datasets.foreach { case (id, filename) =>
      println(s"Fetching data for dataset ID $id...")
      api.fetchData(id, formattedStartTime, formattedEndTime, filename)
    }
  }

  @tailrec
  def readDateTime(prompt: String, formatter: DateTimeFormatter): LocalDateTime = {
    println(prompt)
    Try(LocalDateTime.parse(readLine(), formatter)) match {
      case Failure(e: DateTimeParseException) =>
        println(s"Invalid date format, please use the correct format: yyyy-MM-dd HH:mm")
        readDateTime(prompt, formatter)
      case Failure(e) =>
        println(s"Unexpected error: ${e.getMessage}")
        readDateTime(prompt, formatter)
      case scala.util.Success(dateTime) =>
        dateTime
    }
  }
  // Function to monitor and control energy sources, allowing selection between solar, wind, and hydro
  def monitorAndControl(): Unit = {
    println("Monitoring and controlling energy sources...")

    val solarController = new EnergyController("./solar_data.json")
    val windController = new EnergyController("./wind_data.json")
    val hydroController = new EnergyController("./hydro_data.json")

    println("Select the energy source to monitor and control:")
    println("1: Solar")
    println("2: Wind")
    println("3: Hydro")

    readIntSafely() match {
      case Right(choice) => handleChoice(choice, solarController, windController, hydroController)
      case Left(error) =>
        println(error)
        monitorAndControl()
    }
  }

  // Safely reads an integer from standard input, returning either the integer or an error message
  def readIntSafely(): Either[String, Int] = {
    Try(StdIn.readLine().toInt).toEither.left.map(_ => "Invalid input. Please enter a number (1, 2, or 3).")
  }

  // Handles user's choice of energy source and calls respective controller methods
  def handleChoice(choice: Int, solarController: EnergyController, windController: EnergyController, hydroController: EnergyController): Unit = {
    choice match {
      case 1 =>
        println("Monitoring and controlling Solar Energy:")
        solarController.displayAndOptionallyAdjustOutput()
      case 2 =>
        println("Monitoring and controlling Wind Energy:")
        windController.displayAndOptionallyAdjustOutput()
      case 3 =>
        println("Monitoring and controlling Hydro Energy:")
        hydroController.displayAndOptionallyAdjustOutput()
      case _ =>
        println("Invalid choice, please select a valid number (1, 2, or 3).")
        monitorAndControl()
    }
  }

  // Function to view energy data for solar, hydro, and wind sources
  def viewEnergy(): Unit = {
    val viewer = new EnergyDataViewer(
      "./solar_data.json",
      "./hydro_data.json",
      "./wind_data.json"
    )
    viewer.viewEnergyData()
  }

  // Analyzes energy data based on user-defined energy type and granularity
  def analyzeData(): Unit = {
    val energyTypes = Map(1 -> "solar", 2 -> "hydro", 3 -> "wind")
    val granularities = Map(1 -> "hourly", 2 -> "daily", 3 -> "weekly", 4 -> "monthly")

    println("Select the type of energy to analyze:")
    energyTypes.foreach { case (key, value) => println(s"$key: $value") }
    val energyTypeOption = energyTypes.get(StdIn.readInt())

    if (energyTypeOption.isEmpty) {
      println("Invalid energy type selection.")
      return
    }
    val energyType = energyTypeOption.get

    println("Select the granularity of the data:")
    granularities.foreach { case (key, value) => println(s"$key: $value") }
    val granularityOption = granularities.get(StdIn.readInt())

    if (granularityOption.isEmpty) {
      println("Invalid granularity selection.")
      return
    }
    val granularity = granularityOption.get

    val dateFormat = granularity match {
      case "monthly" => "yyyy-MM"
      case "daily"   => "yyyy-MM-dd"
      case "hourly"  => "yyyy-MM-dd HH:mm"
      case "weekly"  => "yyyy-MM-dd"
    }

    val startDateTime = requestDateInput(s"Enter the starting date for $granularity analysis ($dateFormat):", dateFormat, granularity == "hourly")

    val endDateTime = granularity match {
      case "monthly" => YearMonth.parse(startDateTime.format(DateTimeFormatter.ofPattern("yyyy-MM"))).atEndOfMonth().atTime(23, 59)
      case "daily"   => startDateTime.plusDays(1).minusSeconds(1)
      case "hourly"  => requestDateInput("Enter the ending date and time for hourly analysis (yyyy-MM-dd HH:mm):", dateFormat, true)
      case "weekly"  => startDateTime.plusWeeks(1).minusSeconds(1)
    }

    val fileMap = Map(
      "solar" -> "./solar_data.json",
      "hydro" -> "./hydro_data.json",
      "wind" -> "./wind_data.json"
    )

    fileMap.get(energyType) match {
      case Some(fileName) =>
        val dataAnalyzer = new DataAnalyzer()
        dataAnalyzer.analyze(fileName, granularity, startDateTime, endDateTime)
      case None =>
        println("Invalid energy type selected.")
    }
  }

  // Requests and validates date inputs according to specified format
  def requestDateInput(prompt: String, dateFormat: String, isDateTime: Boolean): LocalDateTime = {
    println(prompt)
    val dateString = StdIn.readLine()
    validateDateTime(dateString, dateFormat, isDateTime) match {
      case Some(date) => date
      case None =>
        println(s"Invalid date format. Please enter a date in the correct format ($dateFormat):")
        requestDateInput(prompt, dateFormat, isDateTime)
    }
  }

  // Safely reads a date and time from standard input with a default formatter
  def validateDateTime(input: String, dateFormat: String, isDateTime: Boolean): Option[LocalDateTime] = {
    try {
      if (isDateTime) {
        Some(LocalDateTime.parse(input, DateTimeFormatter.ofPattern(dateFormat)))
      } else {
        dateFormat match {
          case "yyyy-MM" =>
            Some(YearMonth.parse(input, DateTimeFormatter.ofPattern(dateFormat)).atDay(1).atStartOfDay())
          case _ =>
            Some(LocalDate.parse(input, DateTimeFormatter.ofPattern(dateFormat)).atStartOfDay())
        }
      }
    } catch {
      case e: DateTimeParseException =>
        None
    }
  }

  // Detects and handles issues for solar, hydro, or wind energy sources
  implicit val defaultFormatter: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm")

  def readDateTime(implicit formatter: DateTimeFormatter): LocalDateTime = {
    @tailrec
    def attemptRead(): LocalDateTime = {
      println("Enter date and time:")
      Try(LocalDateTime.parse(readLine(), formatter)) match {
        case Success(dateTime) => dateTime
        case Failure(_) =>
          println(s"Invalid date format, please use the correct format: yyyy-MM-dd HH:mm")
          attemptRead()
      }
    }
    attemptRead()
  }

  def detectAndHandleIssues(): Unit = {
    println("Detecting and handling issues...")

    val energyTypes = Map(
      1 -> ("solar", "./solar_data.json"),
      2 -> ("hydro", "./hydro_data.json"),
      3 -> ("wind", "./wind_data.json")
    )

    println("Select the type of energy to detect 1" +
      "issues (1: solar, 2: hydro, 3: wind):")
    energyTypes.foreach { case (key, (typeLabel, _)) => println(s"$key: $typeLabel") }

    val energyTypeKey = readLine()
    val selectedEnergy = Try(energyTypeKey.toInt).toOption.flatMap(energyTypes.get)

    selectedEnergy match {
      case Some((energyType, filePath)) =>
        val issueDetector = new IssueDetector(energyType, filePath)
        issueDetector.detectAndHandleIssues()
      case None =>
        println("Invalid input. Please enter a valid number corresponding to the energy type.")
    }
  }


}
