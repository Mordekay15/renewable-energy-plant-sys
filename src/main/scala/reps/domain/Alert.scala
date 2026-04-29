package reps.domain

import java.time.LocalDateTime

sealed trait Severity { def label: String }
object Severity {
  case object Info extends Severity { val label = "INFO" }
  case object Warning extends Severity { val label = "WARNING" }
  case object Critical extends Severity { val label = "CRITICAL" }
}

sealed trait AlertKind { def code: String }
object AlertKind {
  case object LowEnergyOutput extends AlertKind { val code = "LOW_OUTPUT" }
  case object EquipmentMalfunction extends AlertKind { val code = "MALFUNCTION" }
  case object MaintenanceMode extends AlertKind { val code = "MAINTENANCE" }
  case object StorageDepleted extends AlertKind { val code = "STORAGE_LOW" }
  case object CapacityExceeded extends AlertKind { val code = "OVER_CAPACITY" }
}

final case class Alert(
    raisedAt: LocalDateTime,
    source: EnergySource,
    kind: AlertKind,
    severity: Severity,
    message: String
) {
  override def toString: String =
    f"[${severity.label}%-8s] ${raisedAt} ${source.shortCode}%-5s ${kind.code}%-13s :: $message"
}