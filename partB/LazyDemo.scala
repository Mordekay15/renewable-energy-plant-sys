object LazyDemo {

  final case class EnergyRecord(timestamp: String, energyOutputMWh: Double, capacityMW: Double) {
    def utilisation: Double =
      if (capacityMW <= 0d) 0d else (energyOutputMWh / capacityMW).max(0d).min(1d)
  }

  final class PlantReport(records: List[EnergyRecord]) {

    private var _heavyCalls = 0
    def heavyCalls: Int = _heavyCalls

    private def heavy[A](label: String, costMs: Int)(body: => A): A = {
      _heavyCalls += 1
      println(s"  [running] $label (~$costMs ms)")
      Thread.sleep(costMs.toLong)
      body
    }

    val currentOutput: Double =
      records.lastOption.map(_.energyOutputMWh).getOrElse(0.0)

    lazy val tenYearProjection: Double =
      heavy("10-year efficiency projection", 800) {
        val avgUtil =
          if (records.isEmpty) 0.0
          else records.iterator.map(_.utilisation).sum / records.size
        avgUtil * math.pow(0.985, 10) // 1.5 % de-rating per year
      }

    lazy val decadeWeatherSummary: Map[String, Double] =
      heavy("decade weather aggregation", 1200) {
        Map(
          "avg_irradiance_w_per_m2" -> 540.0,
          "avg_wind_speed_m_s"      -> 7.4,
          "avg_rainfall_mm_per_yr"  -> 612.0
        )
      }
  }


  def main(args: Array[String]): Unit = {
    val records = List(
      EnergyRecord("2024-04-12T12:00", 35.0, 50.0),
      EnergyRecord("2024-04-12T13:00", 14.0, 30.0)
    )

    println("=== Scenario A: operator only checks the current output ===")
    val a = new PlantReport(records)
    println(s"   currentOutput = ${a.currentOutput} MWh")
    println(s"   heavy work performed: ${a.heavyCalls}    <-- zero, as expected")

    println("\n=== Scenario B: operator drills into the 10-year projection ===")
    val b = new PlantReport(records)
    println(f"   projection (1st access) = ${b.tenYearProjection}%.4f")
    println(f"   projection (2nd access) = ${b.tenYearProjection}%.4f   // cached")
    println(s"   heavy work performed: ${b.heavyCalls}    <-- one, fired only once")

    println("\n=== Scenario C: operator queries the full weather history too ===")
    val c = new PlantReport(records)
    c.tenYearProjection
    c.decadeWeatherSummary
    c.decadeWeatherSummary // cached, no re-run
    println(s"   weather (cached)   = ${c.decadeWeatherSummary}")
    println(s"   heavy work performed: ${c.heavyCalls}    <-- two, each lazy field fired exactly once")

    println("\nConclusion: laziness lets us ship features that would be too")
    println("expensive to compute up-front, paying only for what the operator asks for.")
  }
}