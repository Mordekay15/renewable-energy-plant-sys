package reps.analysis

import reps.domain.{EnergyRecord, EnergySource, Period}

import java.time.temporal.IsoFields
import java.time.{LocalDate, LocalDateTime}

object Filters {

  def bySource(source: EnergySource)(records: List[EnergyRecord]): List[EnergyRecord] =
    records.filter(_.source == source)

  def where(p: EnergyRecord => Boolean)(records: List[EnergyRecord]): List[EnergyRecord] =
    records.filter(p)

  def inRange(from: LocalDateTime, to: LocalDateTime)(records: List[EnergyRecord]): List[EnergyRecord] =
    records.filter(r => !r.timestamp.isBefore(from) && !r.timestamp.isAfter(to))

  def inSamePeriod(period: Period, anchor: LocalDateTime)(records: List[EnergyRecord]): List[EnergyRecord] =
    records.filter(r => sameBucket(period, anchor, r.timestamp))

  def groupByPeriod(period: Period)(records: List[EnergyRecord]): Map[String, List[EnergyRecord]] =
    records.groupBy(r => bucketKey(period, r.timestamp))

  def search(term: String)(records: List[EnergyRecord]): List[EnergyRecord] = {
    val needle = term.trim.toLowerCase
    if (needle.isEmpty) records
    else records.filter { r =>
      r.timestamp.toString.toLowerCase.contains(needle) ||
      r.source.shortCode.toLowerCase.contains(needle) ||
      r.status.label.toLowerCase.contains(needle)
    }
  }

  def onDate(date: LocalDate)(records: List[EnergyRecord]): List[EnergyRecord] =
    records.filter(_.timestamp.toLocalDate == date)

  sealed trait SortField
  object SortField {
    case object Timestamp  extends SortField
    case object Output     extends SortField
    case object Capacity   extends SortField
    case object Storage    extends SortField
    case object Source     extends SortField
  }

  def sortBy(field: SortField, ascending: Boolean = true)(records: List[EnergyRecord]): List[EnergyRecord] = {
    val ord: Ordering[EnergyRecord] = field match {
      case SortField.Timestamp => Ordering.by(_.timestamp.toString)
      case SortField.Output    => Ordering.by(_.energyOutputMWh)
      case SortField.Capacity  => Ordering.by(_.capacityMW)
      case SortField.Storage   => Ordering.by(_.storageMWh)
      case SortField.Source    => Ordering.by(_.source.shortCode)
    }
    val sorted = records.sorted(ord)
    if (ascending) sorted else sorted.reverse
  }

  private def bucketKey(p: Period, ts: LocalDateTime): String = p match {
    case Period.Hourly  => f"${ts.getYear}-${ts.getMonthValue}%02d-${ts.getDayOfMonth}%02d ${ts.getHour}%02d:00"
    case Period.Daily   => ts.toLocalDate.toString
    case Period.Weekly  =>
      val week = ts.get(IsoFields.WEEK_OF_WEEK_BASED_YEAR)
      val year = ts.get(IsoFields.WEEK_BASED_YEAR)
      f"$year-W$week%02d"
    case Period.Monthly => f"${ts.getYear}-${ts.getMonthValue}%02d"
  }

  private def sameBucket(p: Period, a: LocalDateTime, b: LocalDateTime): Boolean =
    bucketKey(p, a) == bucketKey(p, b)
}