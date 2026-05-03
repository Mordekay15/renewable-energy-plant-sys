package reps.domain

// Domain model for the Renewable Energy Plant System, including energy sources, records, plant state, and time periods.

sealed trait EquipmentStatus {
  def label: String
}

object EquipmentStatus {
  case object Operational extends EquipmentStatus { val label = "OPERATIONAL" }
  case object Maintenance extends EquipmentStatus { val label = "MAINTENANCE" }
  case object Malfunction extends EquipmentStatus { val label = "MALFUNCTION" }

  def fromString(raw: String): Either[String, EquipmentStatus] =
    raw.trim.toUpperCase match {
      case "OPERATIONAL" => Right(Operational)
      case "MAINTENANCE" => Right(Maintenance)
      case "MALFUNCTION" => Right(Malfunction)
      case other => Left(s"Unknown equipment status: '$other'.")
    }
}