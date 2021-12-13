package day12

object Solution {

  def parse(line: String): (String, String) = line match {
    case s"$a-$b" => (a, b)
  }

  def getAdjacencyList(input: Seq[(String, String)]): Map[String, Seq[String]] =
    (input ++ input.map(e => (e._2, e._1))).groupBy(_._1).view.mapValues(_.map(_._2)).toMap

  def getPaths(input: Map[String, Seq[String]]): Seq[Seq[String]] = {

    def helper(currentNode: String, visited: Set[String], path: Seq[String]): Seq[Seq[String]] = {
      if (visited.contains(currentNode))
        Seq()
      else if (currentNode == "end")
        Seq(path :+ "end")
      else {
        val currentVisited = if ("[a-z]+".r.matches(currentNode)) visited + currentNode else visited
        input(currentNode).foldLeft(Seq(Seq[String]())) { case (paths, next) =>
          paths ++ helper(next, currentVisited, path :+ currentNode)
        }
      }
    }

    helper("start", Set.empty, Seq.empty).filterNot(_.isEmpty)

  }

}
