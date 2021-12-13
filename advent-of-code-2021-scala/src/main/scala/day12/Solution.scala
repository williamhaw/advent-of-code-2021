package day12

object Solution {

  def parse(line: String): (String, String) = line match {
    case s"$a-$b" => (a, b)
  }

  val smallCavePattern                   = "[a-z]+".r
  def isSmallCave(cave: String): Boolean = smallCavePattern.matches(cave)

  def getAdjacencyList(input: Seq[(String, String)]): Map[String, Seq[String]] =
    (input ++ input.map(e => (e._2, e._1)))
      .filterNot(e => e._2 == "start")
      .groupBy(_._1)
      .view
      .mapValues(_.map(_._2))
      .toMap

  def getPaths(input: Map[String, Seq[String]]): Seq[Seq[String]] = {

    def helper(currentNode: String, visited: Set[String], path: Seq[String]): Seq[Seq[String]] =
      if (visited.contains(currentNode))
        Seq()
      else if (currentNode == "end")
        Seq(path :+ "end")
      else {
        val currentVisited = if (isSmallCave(currentNode)) visited + currentNode else visited
        input(currentNode).foldLeft(Seq(Seq[String]())) { case (paths, next) =>
          paths ++ helper(next, currentVisited, path :+ currentNode)
        }
      }
    helper("start", Set.empty, Seq.empty).filterNot(_.isEmpty)
  }

  def getPathsSmallCaveTwice(input: Map[String, Seq[String]]): Seq[Seq[String]] = {

    def helper(currentNode: String, visited: Map[String, Int], path: Seq[String]): Seq[Seq[String]] = {
      if ((visited.values.size > 0 && visited.values.max > 2) || visited.values.count(_ >= 2) > 1)
        Seq()
      else if (currentNode == "end")
        Seq(path :+ "end")
      else {
        val currentVisited =
          if (isSmallCave(currentNode)) visited + (currentNode -> (visited(currentNode) + 1)) else visited
        input(currentNode).foldLeft(Seq(Seq[String]())) { case (paths, next) =>
          paths ++ helper(next, currentVisited, path :+ currentNode)
        }
      }
    }

    val ret = helper("start", Map.empty.withDefaultValue(0), Seq.empty).filterNot(_.isEmpty)
    ret
  }

}
