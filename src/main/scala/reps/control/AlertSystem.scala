package reps.control

import reps.domain._

// Alert system for the Renewable Energy Plant System, defining rules for generating alerts based on energy records, and evaluating those rules against a list of records to produce actionable alerts for operators.

object AlertSystem {

  type Rule = EnergyRecord => Option[Alert]

  def lowOutputRule(thresholdRatio: Double): Rule = (r: EnergyRecord) =>
    if (r.status == EquipmentStatus.Operational
        && r.capacityMW > 0d
        && r.utilisation < thresholdRatio
        && expectsProduction(r)) {
      Some(Alert(
        raisedAt = r.timestamp,
        source = r.source,
        kind = AlertKind.LowEnergyOutput,
        severity = if (r.utilisation < thresholdRatio / 2) Severity.Critical else Severity.Warning,
        message = f"Output ${r.energyOutputMWh}%.2f MWh is only ${r.utilisation * 100}%.1f%% of capacity (threshold ${thresholdRatio * 100}%.0f%%)."
      ))
    } else None

  val malfunctionRule: Rule = (r: EnergyRecord) =>
    if (r.status == EquipmentStatus.Malfunction)
      Some(Alert(
        raisedAt = r.timestamp,
        source = r.source,
        kind = AlertKind.EquipmentMalfunction,
        severity = Severity.Critical,
        message = "Equipment reports MALFUNCTION — dispatch maintenance crew."
      ))
    else None

  val maintenanceRule: Rule = (r: EnergyRecord) =>
    if (r.status == EquipmentStatus.Maintenance)
      Some(Alert(
        raisedAt = r.timestamp,
        source = r.source,
        kind = AlertKind.MaintenanceMode,
        severity = Severity.Info,
        message = "Equipment is offline for maintenance."
      ))
    else None

  def storageLowRule(floorMWh: Double): Rule = (r: EnergyRecord) =>
    if (r.storageMWh < floorMWh)
      Some(Alert(
        raisedAt = r.timestamp,
        source = r.source,
        kind = AlertKind.StorageDepleted,
        severity = Severity.Warning,
        message = f"Storage buffer at ${r.storageMWh}%.2f MWh (floor $floorMWh%.2f)."
      ))
    else None

  val overCapacityRule: Rule = (r: EnergyRecord) =>
    if (r.energyOutputMWh > r.capacityMW * 1.05)
      Some(Alert(
        raisedAt = r.timestamp,
        source = r.source,
        kind = AlertKind.CapacityExceeded,
        severity = Severity.Warning,
        message = f"Output ${r.energyOutputMWh}%.2f MWh exceeds capacity ${r.capacityMW}%.2f MWh — sensor or model drift?"
      ))
    else None

  def defaultRules(lowThreshold: Double = 0.30, storageFloor: Double = 0.5): List[Rule] =
    List(
      malfunctionRule,
      maintenanceRule,
      lowOutputRule(lowThreshold),
      storageLowRule(storageFloor),
      overCapacityRule
    )

  def evaluate(rules: List[Rule])(records: List[EnergyRecord]): List[Alert] =
    records.flatMap(r => rules.flatMap(rule => rule(r).toList))

  def evaluateDefault(records: List[EnergyRecord]): List[Alert] =
    evaluate(defaultRules())(records)

  private def expectsProduction(r: EnergyRecord): Boolean = r.source match {
    case EnergySource.Solar =>
      val h = r.timestamp.getHour
      h >= 7 && h <= 18
    case EnergySource.Wind | EnergySource.Hydro =>
      true
  }
}