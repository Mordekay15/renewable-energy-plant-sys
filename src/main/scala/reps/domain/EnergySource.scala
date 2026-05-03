package reps.domain

// Domain model for the Renewable Energy Plant System, including energy sources, records, plant state, and time periods.

sealed trait EnergySource {
  def name: String
  def shortCode: String
}

object EnergySource {
  case object Solar extends EnergySource {
    val name = "Solar Panels"
    val shortCode = "SOLAR"
  }
  case object Wind extends EnergySource {
    val name = "Wind Turbines"
    val shortCode = "WIND"
  }
  case object Hydro extends EnergySource {
    val name = "Hydro Plant"
    val shortCode = "HYDRO"
  }

  val all: List[EnergySource] = List(Solar, Wind, Hydro)

  def fromString(raw: String): Either[String, EnergySource] =
    raw.trim.toUpperCase match {
      case "SOLAR" => Right(Solar)
      case "WIND"  => Right(Wind)
      case "HYDRO" => Right(Hydro)
      case other => Left(s"Unknown energy source: '$other'. Expected SOLAR | WIND | HYDRO.")
    }
}