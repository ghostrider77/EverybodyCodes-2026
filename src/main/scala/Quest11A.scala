object Quest11A:
  case class PortalRing(portals: Int, shortcuts: List[(Int, Int)])

  class Graph(val nrNodes: Int, val adjacencyList: Map[Int, List[Int]])

  object Graph:
    def apply(ring: PortalRing): Graph = {
      val nrNodes: Int = ring.portals
      val ringEdges: List[(Int, Int)] =
        (0 until ring.portals).toList.flatMap(k => List((k, (k + 1) % nrNodes), ((k + 1) % nrNodes, k)))
      val edges: List[(Int, Int)] = ringEdges ::: ring.shortcuts.flatMap((a, b) => List((a - 1, b - 1), (b - 1, a - 1)))
      val adjacencyList: Map[Int, List[Int]] = edges.groupMap(_._1)(_._2)
      new Graph(nrNodes, adjacencyList)
    }

  private def readInput(lines: List[String]): PortalRing = {
    def parseShortCut(line: String): (Int, Int) = line match
      case s"$a $b" => (a.toInt, b.toInt)
      case _ => throw Exception(s"Malformed shortcut: $line")

    lines match
      case hd :: "" :: rest => hd match
        case s"portals: $portals" => PortalRing(portals.toInt, rest.map(parseShortCut))
      case _ => throw Exception("Malformed input.")
  }

  private def calcShortestPaths(graph: Graph): Vector[Vector[Int]] = {
    val n: Int = graph.nrNodes
    val distances: Array[Array[Double]] = Array.fill(n, n)(Double.PositiveInfinity)
    graph.adjacencyList.foreach { case (i, neighbors) => neighbors.foreach(j => distances(i)(j) = 1) }
    graph.adjacencyList.keysIterator.foreach(v => distances(v)(v) = 0)
    for {
      k <- 0 until n
      i <- 0 until n
      j <- 0 until n
    } distances(i)(j) = math.min(distances(i)(j), distances(i)(k) + distances(k)(j))

    distances.map(_.map(_.toInt).toVector).toVector
  }

  def calcPortalResonance(graph: Graph): Int = {
    val k: Int = graph.nrNodes / 2
    val routeSumWithoutShortcuts: Int =
      if graph.nrNodes % 2 == 0 then graph.nrNodes * k * k else graph.nrNodes * k * (k + 1)
    val distances: Vector[Vector[Int]] = calcShortestPaths(graph)
    val shortCutRouteSum: Int = distances.map(_.sum).sum
    routeSumWithoutShortcuts - shortCutRouteSum
  }

  def main(args: Array[String]): Unit = {
    val lines: Iterator[String] = scala.io.Source.stdin.getLines()
    val ring: PortalRing = readInput(lines.toList)
    val graph = Graph(ring)
    val result: Int = calcPortalResonance(graph)
    println(result)
  }

end Quest11A
