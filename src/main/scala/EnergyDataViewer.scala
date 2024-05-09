import play.api.libs.json.{JsError, JsObject, JsPath, JsSuccess, JsValue, Json}
import scala.io.Source

class EnergyDataViewer(solarPath: String, hydroPath: String, windPath: String) {
  // Method to view energy generation and storage data
  def viewEnergyData(): Unit = {
    println("\n--- View Energy Generation and Storage Data ---")

    // Summing energy data from JSON files
    val solarData = sumEnergyFromJson(solarPath)
    val hydroData = sumEnergyFromJson(hydroPath)
    val windData = sumEnergyFromJson(windPath)
    val totalEnergy = solarData + hydroData + windData

    // Calculating percentage of energy from each source
    val solarPct = 100 * solarData / totalEnergy
    val hydroPct = 100 * hydroData / totalEnergy
    val windPct = 100 * windData / totalEnergy

    // Displaying energy data summary
    println("Source           Total Energy (kWh)    Percentage")
    println("-------------------------------------------------")
    println(f"Solar           ${solarData}%.2f kWh           ${solarPct}%.2f%%")
    println(f"Hydro           ${hydroData}%.2f kWh          ${hydroPct}%.2f%%")
    println(f"Wind            ${windData}%.2f kWh          ${windPct}%.2f%%")
    println("-------------------------------------------------")
    println(f"Total           ${totalEnergy}%.2f kWh")

    // Scaling energy bars for graphical representation
    val maxBarLength = 50
    val solarBarLength = (solarPct / 100 * maxBarLength).toInt
    val hydroBarLength = (hydroPct / 100 * maxBarLength).toInt
    val windBarLength = (windPct / 100 * maxBarLength).toInt

    // Displaying energy bars
    println()
    println("Source    | Energy Bar (scaled to percentage)")
    println("-------------------------------------------")
    println(f"Solar     | ${"#" * solarBarLength}%s ${solarPct}%.2f%% (${solarData}%.2f kWh)")
    println(f"Hydro     | ${"#" * hydroBarLength}%s ${hydroPct}%.2f%% (${hydroData}%.2f kWh)")
    println(f"Wind      | ${"#" * windBarLength}%s ${windPct}%.2f%% (${windData}%.2f kWh)")
  }

  // Method to sum energy data from a JSON file
  private def sumEnergyFromJson(filePath: String): Double = {
    val source = Source.fromFile(filePath)
    try {
      val jsonString = source.mkString
      val json: JsValue = Json.parse(jsonString)

      // Validating JSON structure and summing energy values
      json.validate((JsPath \ "data").read[Seq[JsObject]]) match {
        case JsSuccess(data, _) =>
          data.flatMap { obj =>
            (obj \ "value").validate[Double] match {
              case JsSuccess(value, _) => Some(value)
              case JsError(_) =>
                println("Invalid or missing 'value' in data object")
                None
            }
          }.sum
        case JsError(errors) =>
          println(s"Error parsing data field: $errors")
          0.0
      }
    } catch {
      case e: Exception =>
        println(s"Error reading or parsing JSON file: $e")
        0.0
    } finally {
      source.close()
    }
  }
}
