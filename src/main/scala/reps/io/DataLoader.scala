package reps.io

import reps.domain.{EnergyRecord, EnergySource, EquipmentStatus}

import java.time.LocalDateTime
import scala.util.Random

// Data loading utilities for the Renewable Energy Plant System, including a function 
// to synthesise energy records for a given number of days starting from a specified timestamp. 
// The synthesised data includes realistic profiles for solar, wind, and hydro energy sources, 
//as well as random variations in output and equipment status to simulate real-world conditions.

object DataLoader {
  def synthesise(days: Int, start: LocalDateTime, seed: Long = 42L): List[EnergyRecord] = {
    val rng = new Random(seed)
    val totalHours = days * 24

    @scala.annotation.tailrec
    def loop(hour: Int, acc: List[EnergyRecord]): List[EnergyRecord] =
      if (hour >= totalHours) acc.reverse
      else {
        val ts = start.plusHours(hour.toLong)
        val perSource = EnergySource.all.map(src => makeRecord(ts, src, hour, rng))
        loop(hour + 1, perSource.reverse ::: acc)
      }

    loop(0, Nil)
  }

  private def makeRecord(
      ts: LocalDateTime,
      source: EnergySource,
      hour: Int,
      rng: Random
  ): EnergyRecord = {
    val (capacity, output) = profile(source, hour, rng)
    val storage = (capacity * 0.4 + rng.nextGaussian() * 1.5).max(0d)
    val status  = pickStatus(rng)
    EnergyRecord(ts, source, output, capacity, storage, status)
  }

  private def profile(source: EnergySource, hour: Int, rng: Random): (Double, Double) = {
    val hourOfDay = ((hour % 24) + 24) % 24
    source match {
      case EnergySource.Solar =>
        val cap  = 50.0
        val base = math.max(0d, math.sin(math.Pi * hourOfDay / 24.0)) * cap
        val out  = (base * (0.85 + rng.nextDouble() * 0.15)).max(0d)
        (cap, out)
      case EnergySource.Wind =>
        val cap  = 30.0
        val base = 12.0 + rng.nextGaussian() * 6.0
        val out  = base.max(0d).min(cap)
        (cap, out)
      case EnergySource.Hydro =>
        val cap  = 25.0
        val base = 20.0 + rng.nextGaussian() * 1.5
        val out  = base.max(0d).min(cap)
        (cap, out)
    }
  }

  private def pickStatus(rng: Random): EquipmentStatus = {
    val r = rng.nextDouble()
    if (r < 0.01) EquipmentStatus.Malfunction
    else if (r < 0.04) EquipmentStatus.Maintenance
    else EquipmentStatus.Operational
  }
}