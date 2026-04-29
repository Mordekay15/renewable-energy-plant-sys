package reps.domain

sealed trait Period { def label: String }

object Period {
  case object Hourly  extends Period { val label = "hour"  }
  case object Daily   extends Period { val label = "day"   }
  case object Weekly  extends Period { val label = "week"  }
  case object Monthly extends Period { val label = "month" }

  val all: List[Period] = List(Hourly, Daily, Weekly, Monthly)

  def fromString(raw: String): Either[String, Period] =
    raw.trim.toLowerCase match {
      case "hour" | "hourly" => Right(Hourly)
      case "day"  | "daily" => Right(Daily)
      case "week" | "weekly" => Right(Weekly)
      case "month"| "monthly" => Right(Monthly)
      case other => Left(s"Unknown period '$other'. Use hourly|daily|weekly|monthly.")
    }
}