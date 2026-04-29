package reps

import reps.ui.ConsoleUI

object Main {
  def main(args: Array[String]): Unit = {
    val dataPath = args.headOption.getOrElse("energy_data.csv")
    new ConsoleUI(dataPath).run()
  }
}