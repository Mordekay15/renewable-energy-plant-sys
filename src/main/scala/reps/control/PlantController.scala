package reps.control

import reps.domain._

object PlantController {

  final case class PlantState(
      solarPanelAzimuthDeg: Int,  
      windTurbineYawDeg: Int,  
      hydroFlowPercent: Int,   
      solarOnline: Boolean,
      windOnline: Boolean,
      hydroOnline: Boolean
  )

  object PlantState {
    val initial: PlantState =
      PlantState(
        solarPanelAzimuthDeg = 180,
        windTurbineYawDeg = 0,
        hydroFlowPercent = 75,
        solarOnline = true,
        windOnline = true,
        hydroOnline = true
      )
  }

  def moveSolarPanel(targetAzimuth: Int)(state: PlantState): Either[String, PlantState] =
    if (targetAzimuth < 0 || targetAzimuth > 360)
      Left(s"Azimuth must be in [0, 360], got $targetAzimuth.")
    else
      Right(state.copy(solarPanelAzimuthDeg = targetAzimuth % 360))

  def turnWindTurbine(targetYaw: Int)(state: PlantState): Either[String, PlantState] =
    if (targetYaw < 0 || targetYaw > 360)
      Left(s"Yaw must be in [0, 360], got $targetYaw.")
    else
      Right(state.copy(windTurbineYawDeg = targetYaw % 360))

  def setHydroFlow(percent: Int)(state: PlantState): Either[String, PlantState] =
    if (percent < 0 || percent > 100)
      Left(s"Hydro flow must be in [0, 100]%, got $percent.")
    else
      Right(state.copy(hydroFlowPercent = percent))

  def toggleSource(source: EnergySource, online: Boolean)(state: PlantState): Either[String, PlantState] =
    source match {
      case EnergySource.Solar => Right(state.copy(solarOnline = online))
      case EnergySource.Wind  => Right(state.copy(windOnline  = online))
      case EnergySource.Hydro => Right(state.copy(hydroOnline = online))
    }

  def suggestAdjustment(state: PlantState, recent: List[EnergyRecord]): String =
    if (recent.isEmpty) "No recent telemetry — cannot make a recommendation."
    else {
      val avgUtilisation = recent.map(_.utilisation).sum / recent.size
      val baseline = if (avgUtilisation < 0.20)
        "Average utilisation below 20%: consider scheduling maintenance for ageing assets."
      else if (avgUtilisation < 0.40)
        "Utilisation under 40%: re-orient solar panels toward the sun and re-yaw turbines into the wind."
      else if (avgUtilisation > 0.85)
        "Utilisation above 85%: throttle hydro flow to preserve the reservoir."
      else
        "Plant is operating within optimal range — no immediate change recommended."
      val offlineNote =
        if (state.solarOnline && state.windOnline && state.hydroOnline) ""
        else " (heads-up: at least one source is currently OFFLINE)"
      baseline + offlineNote
    }

  def render(state: PlantState): String =
    s"""+--------------------- Plant state ---------------------+
       | Solar  : ${if (state.solarOnline) "ONLINE " else "OFFLINE"}  azimuth=${state.solarPanelAzimuthDeg}°
       | Wind   : ${if (state.windOnline)  "ONLINE " else "OFFLINE"}  yaw=${state.windTurbineYawDeg}°
       | Hydro  : ${if (state.hydroOnline) "ONLINE " else "OFFLINE"}  flow=${state.hydroFlowPercent}%
       +-------------------------------------------------------+""".stripMargin
}