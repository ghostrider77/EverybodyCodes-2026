object Quest06A:
  import scala.collection.mutable.{Map as MutableMap, Queue as MutableQueue}

  class DirectedGraph(private val adjacencyList: Map[String, List[String]]):
    def reachableFrom(startNode: String): Set[String] = {
      val distances: MutableMap[String, Int] = MutableMap.from(List((startNode, 0)))
      val queue: MutableQueue[(String, Int)] = MutableQueue.from(List((startNode, 0)))
      while queue.nonEmpty do
        val (node: String, dist: Int) = queue.dequeue()
        val unvisitedNeighbors: List[String] = adjacencyList.getOrElse(node, Nil).filterNot(distances.contains)
        unvisitedNeighbors.foreach(distances.update(_, dist + 1))
        queue.enqueueAll(unvisitedNeighbors.map((_, dist + 1)))

      distances.filter { case (_, dist) => dist > 0 }.keysIterator.toSet
    }

  def readGraph(lines: List[String]): DirectedGraph = {
    def parse(line: String): (String, List[String]) = line.split(":").toList match
      case List(node, neighbors) => (node, neighbors.split(",").toList)
      case _ => throw Exception(s"Malformed input $line.")

    DirectedGraph(lines.map(parse).toMap)
  }

  def getEveryIngredientsNeeded(graph: DirectedGraph): Int = {
    val startNode: String = "NULL"
    val ingredients: Set[String] = graph.reachableFrom(startNode)
    ingredients.size
  }

  def main(args: Array[String]): Unit = {
    val lines: Iterator[String] = scala.io.Source.stdin.getLines()
    val graph: DirectedGraph = readGraph(lines.toList)
    val result: Int = getEveryIngredientsNeeded(graph)
    println(result)
  }

end Quest06A
