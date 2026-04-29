package reps.ui

import reps.analysis.Filters.SortField
import reps.analysis.{Filters, Statistics}
import reps.control.AlertSystem
import reps.control.PlantController.PlantState
import reps.control.{PlantController => PC}
import reps.domain._
import reps.io.{CsvIO, DataLoader}

import java.time.{LocalDate, LocalDateTime}
import scala.annotation.tailrec
import scala.io.StdIn

final class ConsoleUI(dataPath: String) {
  private var records: List[EnergyRecord] = Nil
  private var plant:   PlantState         = PlantState.initial

  def run(): Unit = {
    println(banner)
    bootstrap()
    loop()
    println("\nGoodbye.")
  }

  private def bootstrap(): Unit = {
    val (loaded, errors) = CsvIO.readAll(dataPath)
    if (errors.nonEmpty) errors.foreach(e => println(s"  ! $e"))
    if (loaded.nonEmpty) {
      records = loaded
      println(s"Loaded ${loaded.size} records from $dataPath.")
    } else {
      println(s"No data found at $dataPath – generating a synthetic 14-day dataset…")
      val start = LocalDateTime.now().withMinute(0).withSecond(0).withNano(0).minusDays(14)
      val seed = DataLoader.synthesise(days = 14, start = start)
      val ok = CsvIO.writeAll(dataPath, seed)
      if (!ok) println("  ! Failed to persist synthetic data — running in-memory only.")
      records = seed
      println(s"Generated ${seed.size} records.")
    }
  }

  @tailrec
  private def loop(): Unit = {
    println(menu)
    val choice = StdIn.readLine("> ").trim
    choice match {
      case "1" => actionViewLatest(); loop()
      case "2" => actionAnalyse(); loop()
      case "3" => actionSearch(); loop()
      case "4" => actionAlerts(); loop()
      case "5" => actionPlantControl(); loop()
      case "6" => actionReload(); loop()
      case "7" => actionExportFiltered(); loop()
      case "8" => actionRegenerate(); loop()
      case "0" | "q" | "exit" | "quit" => () 
      case other =>
        println(s"Unknown option '$other'.")
        loop()
    }
  }

  private def actionViewLatest(): Unit = {
    val latest = Filters.sortBy(SortField.Timestamp, ascending = false)(records).take(20)
    if (latest.isEmpty) println("(no records)")
    else {
      println("Showing the 20 most recent measurements:")
      printTable(latest)
    }
    val totalsBySource = EnergySource.all.map { s =>
      val list = Filters.bySource(s)(records)
      val totalProd = list.map(_.energyOutputMWh).sum
      val totalCap = list.map(_.capacityMW).sum
      val totalSto = list.map(_.storageMWh).sum
      f"  ${s.shortCode}%-5s  total=${totalProd}%10.2f MWh  capacity-sum=${totalCap}%10.2f MW  storage-sum=${totalSto}%10.2f MWh"
    }.mkString("\n")
    println("\nAggregate totals over the dataset:")
    println(totalsBySource)
  }

  private def actionAnalyse(): Unit = {
    println("Pick an energy source: 1) Solar  2) Wind  3) Hydro  4) All")
    val src = StdIn.readLine("> ").trim match {
      case "1" => Some(EnergySource.Solar)
      case "2" => Some(EnergySource.Wind)
      case "3" => Some(EnergySource.Hydro)
      case "4" => None
      case _   => println("Defaulting to ALL sources."); None
    }
    println("Pick a period: 1) Hourly  2) Daily  3) Weekly  4) Monthly  5) None (whole dataset)")
    val period = StdIn.readLine("> ").trim match {
      case "1" => Some(Period.Hourly)
      case "2" => Some(Period.Daily)
      case "3" => Some(Period.Weekly)
      case "4" => Some(Period.Monthly)
      case _ => None
    }

    val filteredBySource: List[EnergyRecord] = src match {
      case Some(s) => Filters.bySource(s)(records)
      case None => records
    }

    val groups: Map[String, List[EnergyRecord]] = period match {
      case Some(p) => Filters.groupByPeriod(p)(filteredBySource)
      case None => Map("ALL" -> filteredBySource)
    }

    if (groups.isEmpty || groups.forall(_._2.isEmpty)) {
      println("No data matches the selected filters.")
    } else {
      groups.toList.sortBy(_._1).foreach { case (key, group) =>
        val outputs = group.map(_.energyOutputMWh)
        val s = Statistics.summary(outputs)
        println(s"\n--- bucket [$key] (${group.size} records) ---")
        println(s.pretty)
      }
    }

    println("\nSort the records? 1) by timestamp  2) by output  3) by source  (anything else = skip)")
    StdIn.readLine("> ").trim match {
      case "1" =>
        val sorted = Filters.sortBy(SortField.Timestamp, ascending = true)(filteredBySource)
        printTable(sorted.take(20))
      case "2" =>
        val sorted = Filters.sortBy(SortField.Output, ascending = false)(filteredBySource)
        printTable(sorted.take(20))
      case "3" =>
        val sorted = Filters.sortBy(SortField.Source, ascending = true)(filteredBySource)
        printTable(sorted.take(20))
      case _ => ()
    }
  }

  private def actionSearch(): Unit = {
    println("Search by:")
    println("  1) date (DD/MM/YYYY)")
    println("  2) free-text (timestamp / source / status)")
    StdIn.readLine("> ").trim match {
      case "1" =>
        val raw = StdIn.readLine("Date: ")
        Parsers.parseUserDate(raw) match {
          case Left(err)   => println(err)
          case Right(date) => searchByDate(date)
        }
      case "2" =>
        val term = StdIn.readLine("Term: ")
        val hits = Filters.search(term)(records)
        if (hits.isEmpty) println(s"No records match '$term'.")
        else {
          println(s"Found ${hits.size} matching record(s):")
          printTable(hits.take(40))
        }
      case _ => println("Cancelled.")
    }
  }

  private def searchByDate(date: LocalDate): Unit = {
    val hits = Filters.onDate(date)(records)
    if (hits.isEmpty)
      println(s"No available data for the selected date (${date.format(Parsers.userDateFormatter)}). Please choose another date.")
    else {
      println(s"Found ${hits.size} record(s) on ${date.format(Parsers.userDateFormatter)}:")
      printTable(hits)
      val outputs = hits.map(_.energyOutputMWh)
      println("\nDescriptive statistics for the day's energy output:")
      println(Statistics.summary(outputs).pretty)
    }
  }

  private def actionAlerts(): Unit = {
    val alerts = AlertSystem.evaluateDefault(records)
    if (alerts.isEmpty) println("No alerts — every reading is within tolerance.")
    else {
      val byKind = alerts.groupBy(_.kind).view.mapValues(_.size).toMap
      println(s"${alerts.size} alert(s) raised:")
      byKind.toList.sortBy(-_._2).foreach { case (k, n) => println(f"  ${k.code}%-13s : $n") }
      println("\nMost recent 15 alerts:")
      alerts.sortBy(_.raisedAt.toString).reverse.take(15).foreach(a => println(s"  $a"))
    }
  }

  private def actionPlantControl(): Unit = {
    println(PC.render(plant))
    val recent = Filters.sortBy(SortField.Timestamp, ascending = false)(records).take(72)
    println("Suggestion: " + PC.suggestAdjustment(plant, recent))
    println()
    println("  1) Move solar panels (azimuth)")
    println("  2) Yaw wind turbines")
    println("  3) Set hydro flow %")
    println("  4) Toggle a source online/offline")
    println("  5) Back")
    StdIn.readLine("> ").trim match {
      case "1" =>
        val raw = StdIn.readLine("New azimuth (0..360): ")
        Parsers.parseInt(raw).flatMap(d => PC.moveSolarPanel(d)(plant)) match {
          case Left(err) => println(err)
          case Right(s) => plant = s; println(PC.render(plant))
        }
      case "2" =>
        val raw = StdIn.readLine("New yaw (0..360): ")
        Parsers.parseInt(raw).flatMap(d => PC.turnWindTurbine(d)(plant)) match {
          case Left(err) => println(err)
          case Right(s) => plant = s; println(PC.render(plant))
        }
      case "3" =>
        val raw = StdIn.readLine("Flow %: ")
        Parsers.parseInt(raw).flatMap(d => PC.setHydroFlow(d)(plant)) match {
          case Left(err) => println(err)
          case Right(s) => plant = s; println(PC.render(plant))
        }
      case "4" =>
        val raw = StdIn.readLine("Source (SOLAR/WIND/HYDRO): ")
        EnergySource.fromString(raw) match {
          case Left(err) => println(err)
          case Right(src) =>
            val state = StdIn.readLine("Online? (y/n): ").trim.toLowerCase.startsWith("y")
            PC.toggleSource(src, state)(plant) match {
              case Left(err) => println(err)
              case Right(s) => plant = s; println(PC.render(plant))
            }
        }
      case _ => ()
    }
  }

  private def actionReload(): Unit = {
    val (loaded, errors) = CsvIO.readAll(dataPath)
    errors.foreach(e => println(s"  ! $e"))
    records = loaded
    println(s"Reloaded ${loaded.size} records.")
  }

  private def actionExportFiltered(): Unit = {
    val raw = StdIn.readLine("Filter term (blank = export all): ")
    val toExport = if (raw == null || raw.trim.isEmpty) records else Filters.search(raw)(records)
    val path = StdIn.readLine("Destination CSV path: ").trim
    if (path.isEmpty) {
      println("Cancelled.")
    } else {
      val ok = CsvIO.writeAll(path, toExport)
      println(if (ok) s"Exported ${toExport.size} record(s) to $path." else "Export failed.")
    }
  }

  private def actionRegenerate(): Unit = {
    println("This OVERWRITES the data file with a freshly generated dataset.")
    val confirm = StdIn.readLine("Type YES to continue: ").trim
    if (confirm != "YES") {
      println("Cancelled.")
    } else {
      val raw   = StdIn.readLine("How many days? ")
      Parsers.parseInt(raw) match {
        case Left(err) => println(err)
        case Right(d) =>
          val start = LocalDateTime.now().withMinute(0).withSecond(0).withNano(0).minusDays(d.toLong)
          val data = DataLoader.synthesise(days = d, start = start)
          val ok = CsvIO.writeAll(dataPath, data)
          if (ok) {
            records = data
            println(s"Regenerated ${data.size} records.")
          } else println("Failed to write file.")
      }
    }
  }

  private def printTable(rows: Seq[EnergyRecord]): Unit = {
    println(f"${"timestamp"}%-19s  ${"src"}%-5s  ${"output"}%9s  ${"capacity"}%9s  ${"storage"}%9s  ${"status"}%-11s")
    println("-" * 78)
    rows.foreach { r =>
      println(
        f"${r.timestamp.toString}%-19s  ${r.source.shortCode}%-5s  ${r.energyOutputMWh}%9.2f  ${r.capacityMW}%9.2f  ${r.storageMWh}%9.2f  ${r.status.label}%-11s"
      )
    }
  }

  private def banner: String =
    """
      |==============================================================
      |  Renewable Energy Plant System
      |==============================================================
      |""".stripMargin

  private def menu: String =
    """
      |Main menu:
      |  1) View latest readings & totals
      |  2) Analyse data (filter / group / statistics / sort)
      |  3) Search records (by date or free-text)
      |  4) Detect issues (alerts)
      |  5) Plant control & adjustment suggestions
      |  6) Reload data from disk
      |  7) Export filtered records to CSV
      |  8) Regenerate synthetic dataset (overwrites file)
      |  0) Quit
      |""".stripMargin
}