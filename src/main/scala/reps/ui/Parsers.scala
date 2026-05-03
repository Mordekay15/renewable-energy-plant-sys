package reps.ui

import java.time.{LocalDate, LocalDateTime}
import java.time.format.{DateTimeFormatter, DateTimeParseException}

// Utility object for parsing user input into domain types, with error handling and user-friendly messages.

object Parsers {
  val userDateFormatter: DateTimeFormatter = DateTimeFormatter.ofPattern("dd/MM/yyyy")

  def parseUserDate(raw: String): Either[String, LocalDate] = {
    val trimmed = if (raw == null) "" else raw.trim
    if (trimmed.isEmpty) Left("Empty date. Please enter a date in the format 'DD/MM/YYYY'.")
    else {
      try Right(LocalDate.parse(trimmed, userDateFormatter))
      catch {
        case _: DateTimeParseException =>
          val today = LocalDate.now()
          val hint  = today.format(userDateFormatter)
          Left(
            s"Invalid date format. Please enter the date in the format 'DD/MM/YYYY'. " +
              s"For example, enter '$hint' for ${today.format(DateTimeFormatter.ofPattern("MMMM d, yyyy"))}."
          )
      }
    }
  }

  def parseUserDateTime(raw: String): Either[String, LocalDateTime] = {
    val trimmed = if (raw == null) "" else raw.trim
    if (trimmed.isEmpty) Left("Empty date/time.")
    else {
      val withTime = DateTimeFormatter.ofPattern("dd/MM/yyyy HH:mm")
      try Right(LocalDateTime.parse(trimmed, withTime))
      catch {
        case _: DateTimeParseException =>
          parseUserDate(trimmed).map(_.atStartOfDay())
      }
    }
  }

  def parseInt(raw: String): Either[String, Int] =
    try Right(raw.trim.toInt)
    catch { case _: NumberFormatException => Left(s"'$raw' is not an integer.") }
  def parseDouble(raw: String): Either[String, Double] =
    try Right(raw.trim.toDouble)
    catch { case _: NumberFormatException => Left(s"'$raw' is not a number.") }
}