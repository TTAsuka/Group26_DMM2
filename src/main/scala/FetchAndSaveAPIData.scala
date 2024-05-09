import java.net.{HttpURLConnection, URL}
import java.io.{BufferedReader, InputStreamReader, PrintWriter}
import play.api.libs.json._

class FetchAndSaveAPIData(baseUrl: String, apiKey: String, format: String = "json", pageSize: Int = 20000) {

  // Method to fetch data from an API and save it to a file
  def fetchData(datasetId: Int, startTime: String, endTime: String, filePath: String): Unit = {
    val url = new URL(s"$baseUrl?datasets=$datasetId&startTime=$startTime&endTime=$endTime&format=$format&pageSize=$pageSize")
    val connection = url.openConnection().asInstanceOf[HttpURLConnection]
    connection.setRequestMethod("GET")
    connection.setRequestProperty("x-api-key", apiKey)

    try {
      val reader = new BufferedReader(new InputStreamReader(connection.getInputStream))
      val response = Stream.continually(reader.readLine()).takeWhile(_ != null).mkString("\n")
      val jsonData = Json.parse(response)
      writeDataToFile(jsonData, filePath)
      reader.close()
    } finally {
      connection.disconnect()
    }
    println(s"Data for dataset $datasetId successfully written to $filePath")
  }

  // Method to write JSON data to a file
  private def writeDataToFile(jsonData: JsValue, filePath: String): Unit = {
    val writer = new PrintWriter(filePath)
    try {
      writer.write(Json.prettyPrint(jsonData))
    } finally {
      writer.close()
    }
  }
}
