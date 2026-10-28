object Quest02C:
  class Graph(val nrNodes: Int, val adjacencyList: Map[Int, List[Int]])

  object Graph:
    def apply(directedEdges: List[(Int, Int)]): Graph =
      val nrNodes: Int = directedEdges.flatMap { case (a, b) => Set(a, b) }.toSet.max
      val adjacencyList: Map[Int, List[Int]] =
        directedEdges.groupMap { case (startNode, _) => startNode } { case (_, endNode) => endNode }
      new Graph(nrNodes, adjacencyList)

  def readGraph(line: String): Graph =
    def readPair(s: String): (Int, Int) = s match
      case s"$a<$b" => (a.toInt, b.toInt)
      case s"$a>$b" => (b.toInt, a.toInt)
      case _ => throw Exception(s"Malformed input $s.")

    val edges: List[(Int, Int)] = line.split(" ").map(readPair).toList
    Graph(edges)

  private def getReachabilityMatrix(graph: Graph): Vector[Vector[Boolean]] =
    val n: Int = graph.nrNodes
    val reachable: Array[Array[Boolean]] = Array.fill(n, n)(false)
    graph.adjacencyList.foreach { case (i, neighbors) => neighbors.foreach(j => reachable(i - 1)(j - 1) = true) }
    for {
      k <- 0 until n
      i <- 0 until n
      j <- 0 until n
    } reachable(i)(j) = reachable(i)(j) || (reachable(i)(k) && reachable(k)(j))

    reachable.map(_.toVector).toVector

  private def orderAlloys(orderingGraph: Graph): List[Int] =
    val reachability: Vector[Vector[Boolean]] = getReachabilityMatrix(orderingGraph)
    (1 to orderingGraph.nrNodes).toList.sortWith { case (a, b) => reachability(a - 1)(b - 1) }

  def getAlloyGroupChecksum(graphs: List[Graph]): Long =
    def getGroupNumber(graph: Graph): Long = orderAlloys(graph).reverse.mkString.toLong
    graphs.map(getGroupNumber).sum

  def main(args: Array[String]): Unit =
    val lines: Iterator[String] = scala.io.Source.stdin.getLines()
    val graphs: List[Graph] = lines.map(readGraph).toList
    val result: Long = getAlloyGroupChecksum(graphs)
    println(result)

end Quest02C
