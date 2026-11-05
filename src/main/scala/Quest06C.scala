object Quest06C:
  import scala.collection.mutable.{Map as MutableMap, Queue as MutableQueue}

  case class Edge(startNode: String, endNode: String)

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

  object DirectedGraph:
    def apply(edges: List[Edge]): DirectedGraph = {
      val adjacencyList: Map[String, List[String]] = edges.groupMap(_.startNode) { case Edge(_, endNode) => endNode }
      new DirectedGraph(adjacencyList)
    }

  def readEdges(lines: List[String]): List[Edge] = {
    def parseReverseEdges(line: String): List[Edge] = line.split(":").toList match
      case List(node, rest) =>
        rest.split(",").map(neighbor => Edge(neighbor, node)).toList
      case _ => throw Exception(s"Malformed input $line.")

    lines.flatMap(parseReverseEdges)
  }

  def getIngredientsUserAtLeast2Nulls(graph: DirectedGraph): Int = {
    val null1: Set[String] = graph.reachableFrom("NULLv1")
    val null2: Set[String] = graph.reachableFrom("NULLv2")
    val null3: Set[String] = graph.reachableFrom("NULLv3")

    null1.intersect(null2).union(null1.intersect(null3)).union(null2.intersect(null3)).size
  }

  def main(args: Array[String]): Unit = {
    val lines: Iterator[String] = scala.io.Source.stdin.getLines()
    val edges: List[Edge] = readEdges(lines.toList)
    val graph = DirectedGraph(edges)
    val result: Int = getIngredientsUserAtLeast2Nulls(graph)
    println(result)
  }

end Quest06C
