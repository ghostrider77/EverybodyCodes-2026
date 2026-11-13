object Quest08A:
  case class Coord(x: Int, y: Int)
  case class Grid(nrRows: Int, nrCols: Int, heights: Vector[Vector[Int]])

  def parseInput(lines: List[String]): Grid = lines match
    case head :: _ =>
      val nrRows: Int = lines.length
      val nrCols: Int = head.length
      val heights: Vector[Vector[Int]] = lines.map(_.map(h => if h == '.' then 0 else h.asDigit).toVector).toVector
      Grid(nrRows, nrCols, heights)
    case _ => throw Exception("Empty input.")

  private def isVisible(grid: Grid, coord: Coord): Boolean = {
    val Grid(nrRows, nrCols, heights) = grid
    val Coord(x0, y0) = coord

    def isValid(c: Coord): Boolean =
      0 <= c.x && c.x < nrRows && 0 <= c.y && c.y < nrCols

    val height: Int = heights(x0)(y0)
    height > 0 && Iterator
      .from(1)
      .map(k => Coord(x0 - k, y0 - k))
      .takeWhile(isValid)
      .map{ case Coord(x, y) => heights(x)(y) }
      .zipWithIndex
      .forall { case (h, ix) => height >= h - ix  }
  }

  def countVisibleCoordinates(grid: Grid): Int = {
    val Grid(nrRows, nrCols, _) = grid
    (0 until nrRows).flatMap(x => (0 until nrCols).map(y => Coord(x, y))).count(isVisible(grid, _))
  }

  def main(args: Array[String]): Unit = {
    val lines: Iterator[String] = scala.io.Source.stdin.getLines()
    val grid: Grid = parseInput(lines.toList)
    val result: Int = countVisibleCoordinates(grid)
    println(result)
  }

end Quest08A
