import java.io.File

import kantan.csv._
import kantan.csv.ops._
import kantan.csv.generic._

object ScoutActivitySelectorApp extends App {
  if (args.length == 0) println("Must pass one or more CSV files as arguments")
  else {
    val responses = combineDuplicates(readData(args))
    responses.foreach(println(_))
  }

  private def readData(files: Array[String]) = {
    files.flatMap(new File(_).asCsvReader[ActivityChoiceWithOptions](rfc.withHeader).collect { case Right(d) => d }).map(removeOptions)
  }

  private def removeOptions(ac: ActivityChoiceWithOptions) = {
    ActivityChoice(ac.activity, ac.firstPlace.getOrElse(0), ac.secondPlace.getOrElse(0), ac.thirdPlace.getOrElse(0))
  }

  private def combineDuplicates(activities: Array[ActivityChoice]) = {
    activities.groupBy(_.activity).map { vals =>
      val tupled = vals._2.map(ActivityChoice.unapply(_).get)
      val totalFirsts = tupled.map(_._2).sum
      val totalSeconds = tupled.map(_._3).sum
      val totalThirds = tupled.map(_._4).sum
      ActivityChoice(vals._1, totalFirsts, totalSeconds, totalThirds)
    }
  }
}
