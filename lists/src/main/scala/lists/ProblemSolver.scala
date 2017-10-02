package lists

/**
  * Problem described here: http://projecteuler.net/problem=67
  */
object ProblemSolver {

  // sum - is maximum sum of numbers along this path.
  // value - is last number in this path.
  // previous - is link to previous number from this path.
  case class Path(sum: Long, value: Int, previous: Option[Path])

  def extendPath(newValue: Int, previous: Option[Path]): Path = {
    Path(previous.get.sum + newValue, newValue, previous)
  }

  // find optimal solution (path with maximum sum)
  // for every number from the *bottom* line of the triangle 
  def findOptimalPaths(data: List[List[Int]]): List[Path] = {

    def calculateOptimalPathsIter(data: List[List[Int]], previousPath: List[Path]): List[Path] = data match {
      case Nil => previousPath
      case head :: tail => {
        val currentPath = head.zipWithIndex.foldLeft(List[Path]()) {
          case (acc, (elem, index)) => {
            if (index - 1 < 0) {
              acc ++ List(extendPath(elem, Some(previousPath(index))))
            } else if (index >= previousPath.length) {
              acc ++ List(extendPath(elem, Some(previousPath(index - 1))))
            } else if (previousPath(index).sum > previousPath(index - 1).sum) {
              acc ++ List(extendPath(elem, Some(previousPath(index))))
            } else {
              acc ++ List(extendPath(elem, Some(previousPath(index - 1))))
            }
          }
        }
        calculateOptimalPathsIter(tail, currentPath)
      }
    }

    val paths: List[Path] = List(Path(data.head.head, data.head.head, None))
    calculateOptimalPathsIter(data.tail, paths)
  }

  // choose best path that have maximum sum
  def chooseBestPath(paths: List[Path]): Option[Path] = {
    if (paths.isEmpty) None
    else
      Some(paths.maxBy(_.sum))
  }

  def bestSumNumbers(paths: List[Path]): List[Int] = {
    numbersForPath(bestPath(paths))
  }

  // Return list of number along the path (staring from top of triangle to bottom).
  def numbersForPath(path: Option[Path]): List[Int] = {
    def numbersForPathIter(currentPath: Option[Path]): List[Int] = currentPath.isEmpty match {
      case true => List()
      case false => List(currentPath.get.value) ++ numbersForPathIter(currentPath.get.previous)
    }

    numbersForPathIter(path).reverse
  }

  // Select path that have maximum sum. Should return empty list if paths is empty list.
  def bestPath(paths: List[Path]): Option[Path] = {
    if (paths.isEmpty) None
    else
      Some(paths.maxBy(_.sum))
  }
}