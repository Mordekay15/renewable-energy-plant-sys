package reps.domain

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

// Domain model for the Renewable Energy Plant System, including energy sources, records, plant state, and time periods. 
// It defines the core data structures and logic for representing energy production records, 
//including methods for calculating utilisation and formatting for CSV output.

final case class EnergyRecord(
    timestamp: LocalDateTime,
    source: EnergySource,
    energyOutputMWh: Double,   
    capacityMW: Double,         
    storageMWh: Double,         
    status: EquipmentStatus
) {
  def utilisation: Double =
    if (capacityMW <= 0d) 0d else (energyOutputMWh / capacityMW).max(0d).min(1d)

  def toCsvLine: String = {
    val ts = timestamp.format(EnergyRecord.csvFormatter)
    f"$ts,${source.shortCode},$energyOutputMWh%.3f,$capacityMW%.3f,$storageMWh%.3f,${status.label}"
  }
}

object EnergyRecord {
  val csvFormatter: DateTimeFormatter = DateTimeFormatter.ISO_LOCAL_DATE_TIME
  val csvHeader: String = "timestamp,source,energy_output_mwh,capacity_mw,storage_mwh,status"
}