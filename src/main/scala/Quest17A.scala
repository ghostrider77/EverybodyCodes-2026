object Quest17A:
  import scala.annotation.tailrec
  import scala.collection.mutable.Queue as MutableQueue

  case class Cell(x: Int, y: Int)

  case class LavaMap(nrRows: Int, nrCols: Int, solidSquares: Set[Cell])

  def readInput(lines: List[String]): LavaMap = lines match
    case Nil => throw Exception("Empty input.")
    case line :: _ =>
      val nrRows: Int = lines.length
      val nrCols: Int = line.length
      val squares: Set[Cell] =
        lines
          .zipWithIndex
          .flatMap { case (row, x) => row.zipWithIndex.collect { case (char, y) if char == '#' => Cell(x, y) } }
          .toSet
      LavaMap(nrRows, nrCols, squares)

  private def getNeighbors(cell: Cell, squares: Set[Cell]): List[Cell] = {
    val Cell(x, y) = cell
    Iterator(Cell(x - 1, y), Cell(x, y + 1), Cell(x + 1, y), Cell(x, y - 1)).filter(squares.contains).toList
  }

  def findShortestDistance(lavaMap: LavaMap): Int = {
    val LavaMap(nrRows, nrCols, solidSquares) = lavaMap
    val nrNodes: Int = solidSquares.size
    val startNode = Cell(0, 0)
    val targetNode = Cell(nrRows - 1, nrCols - 1)
    val queue: MutableQueue[Cell] = MutableQueue.from(List(startNode))

    @tailrec
    def loop(distances: Map[Cell, Int]): Int = {
      val cell: Cell = queue.dequeue()
      val dist: Int = distances(cell)
      if cell == targetNode then dist
      else {
        val neighbours: List[Cell] = getNeighbors(cell, solidSquares)
        val unvisitedNeighbors: List[Cell] = neighbours.filterNot(distances.contains)
        unvisitedNeighbors.foreach(queue.enqueue)
        loop(distances ++ unvisitedNeighbors.map(neighbor => (neighbor, dist + 1)))
        }
      }

    loop(Map(startNode -> 0))
  }

  def main(args: Array[String]): Unit = {
    val lines: Iterator[String] = scala.io.Source.stdin.getLines()
    val lavaMap: LavaMap = readInput(lines.toList)
    val result: Int = findShortestDistance(lavaMap)
    println(result)
  }

end Quest17A
