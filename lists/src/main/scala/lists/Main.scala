package lists

import ProblemSolver._
import ProblemData._

object Main {
  def main(args: Array[String]) {
    for (data <- Seq(small, medium, large)) {
      val optimalPaths = findOptimalPaths(large)
      val bestPath = chooseBestPath(optimalPaths)
        
      println(s"""[${bestPath.map(_.sum).getOrElse(0)}] ${bestPath.mkString("(", ",", ")")}""")
    }
  }
}
