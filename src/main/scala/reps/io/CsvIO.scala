package reps.io

import reps.domain.{EnergyRecord, EnergySource, EquipmentStatus}

import java.io.{BufferedReader, BufferedWriter, File, FileReader, FileWriter, PrintWriter}
import java.time.LocalDateTime
import java.time.format.DateTimeParseException
import scala.collection.mutable.ListBuffer

// CSV input/output utilities for the Renewable Energy Plant System, including functions 
// to read energy records from a CSV file, write records to a CSV file, and append records 
// to an existing CSV file. The implementation includes error handling for malformed lines 
// I/O issues, and uses a consistent format for timestamps and numeric values in the CSV output.
  
object CsvIO {
  def readAll(path: String): (List[EnergyRecord], List[String]) = {
    val file = new File(path)
    val records = new ListBuffer[EnergyRecord]()
    val errors  = new ListBuffer[String]()

    if (!file.exists()) {
      errors += s"File not found: $path"
      return (records.toList, errors.toList)
    }

    var reader: BufferedReader = null
    try {
      reader = new BufferedReader(new FileReader(file))
      var line: String = reader.readLine()
      var lineNumber = 1
      if (line != null && line.trim.toLowerCase.startsWith("timestamp")) {
        line = reader.readLine()
        lineNumber += 1
      }
      while (line != null) {
        val trimmed = line.trim
        if (trimmed.nonEmpty) {
          parseLine(trimmed, lineNumber) match {
            case Right(rec) => records += rec
            case Left(err)  => errors  += err
          }
        }
        line = reader.readLine()
        lineNumber += 1
      }
    } catch {
      case e: Exception =>
        errors += s"I/O error while reading '$path': ${e.getMessage}"
    } finally {
      if (reader != null) {
        try reader.close()
        catch { case _: Exception => () }
      }
    }

    (records.toList, errors.toList)
  }
  def writeAll(path: String, records: Seq[EnergyRecord], includeHeader: Boolean = true): Boolean = {
    var writer: PrintWriter = null
    try {
      val file = new File(path)
      val parent = file.getParentFile
      if (parent != null && !parent.exists()) {
        parent.mkdirs()
      }
      writer = new PrintWriter(new BufferedWriter(new FileWriter(file, false)))
      if (includeHeader) writer.println(EnergyRecord.csvHeader)
      val it = records.iterator
      while (it.hasNext) {
        writer.println(it.next().toCsvLine)
      }
      true
    } catch {
      case e: Exception =>
        System.err.println(s"I/O error while writing '$path': ${e.getMessage}")
        false
    } finally {
      if (writer != null) {
        try writer.close()
        catch { case _: Exception => () }
      }
    }
  }

  def append(path: String, records: Seq[EnergyRecord]): Boolean = {
    val file = new File(path)
    val needsHeader = !file.exists()
    var writer: PrintWriter = null
    try {
      val parent = file.getParentFile
      if (parent != null && !parent.exists()) parent.mkdirs()
      writer = new PrintWriter(new BufferedWriter(new FileWriter(file, true)))
      if (needsHeader) writer.println(EnergyRecord.csvHeader)
      val it = records.iterator
      while (it.hasNext) writer.println(it.next().toCsvLine)
      true
    } catch {
      case e: Exception =>
        System.err.println(s"I/O error while appending to '$path': ${e.getMessage}")
        false
    } finally {
      if (writer != null) {
        try writer.close()
        catch { case _: Exception => () }
      }
    }
  }

  private def parseLine(line: String, lineNumber: Int): Either[String, EnergyRecord] = {
    val cols = line.split(",", -1).map(_.trim)
    if (cols.length < 6)
      return Left(s"Line $lineNumber: expected 6 columns, found ${cols.length}: '$line'")

    val tsStr     = cols(0)
    val srcStr    = cols(1)
    val outStr    = cols(2)
    val capStr    = cols(3)
    val storeStr  = cols(4)
    val statusStr = cols(5)

    val tsE: Either[String, LocalDateTime] =
      try Right(LocalDateTime.parse(tsStr, EnergyRecord.csvFormatter))
      catch { case _: DateTimeParseException => Left(s"bad timestamp '$tsStr'") }

    val outE = parseDouble(outStr).left.map(_ => s"bad energy output '$outStr'")
    val capE = parseDouble(capStr).left.map(_ => s"bad capacity '$capStr'")
    val stoE = parseDouble(storeStr).left.map(_ => s"bad storage '$storeStr'")

    val srcE = EnergySource.fromString(srcStr)
    val staE = EquipmentStatus.fromString(statusStr)

    val problems = List(tsE, outE, capE, stoE, srcE, staE).collect { case Left(m) => m }
    if (problems.nonEmpty)
      Left(s"Line $lineNumber: ${problems.mkString("; ")}")
    else
      Right(EnergyRecord(
        tsE.toOption.get,
        srcE.toOption.get,
        outE.toOption.get,
        capE.toOption.get,
        stoE.toOption.get,
        staE.toOption.get
      ))
  }

  private def parseDouble(s: String): Either[String, Double] =
    try Right(s.toDouble)
    catch { case _: NumberFormatException => Left(s"not a number: '$s'") }
}