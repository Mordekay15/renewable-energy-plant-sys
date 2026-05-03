package reps

import reps.ui.ConsoleUI

// Group “Scala Johnson” members: 
// Ekaterina Laari
// Polina Antonova
// Jannatun Nur Zinia

// The start of the application
object Main {
  def main(args: Array[String]): Unit = {
    val dataPath = args.headOption.getOrElse("energy_data.csv")
    new ConsoleUI(dataPath).run()
  }
}