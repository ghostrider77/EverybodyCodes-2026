object Quest08B:
  enum Direction:
    case NorthWest, NorthEast, East, SouthEast, SouthWest, West

  case class Coord(x: Int, y: Int)
  case class Grid(nrRows: Int, nrCols: Int, heights: Vector[Vector[Int]])

  def parseInput(lines: List[String]): Grid = lines match
    case head :: _ =>
      val nrRows: Int = lines.length
      val nrCols: Int = head.length
      val heights: Vector[Vector[Int]] = lines.map(_.map(h => if h == '.' then 0 else h.asDigit).toVector).toVector
      Grid(nrRows, nrCols, heights)
    case _ => throw Exception("Empty input.")

  private def ray(coord: Coord, direction: Direction): Int => Coord = {
    val Coord(x, y) = coord
    direction match
      case Direction.NorthWest => (k: Int) => Coord(x - k, y - k)
      case Direction.NorthEast => (k: Int) => Coord(x - k, y + k)
      case Direction.East => (k: Int) => Coord(x, y + 2 * k)
      case Direction.SouthEast => (k: Int) => Coord(x + k, y + k)
      case Direction.SouthWest => (k: Int) => Coord(x + k, y - k)
      case Direction.West => (k: Int) => Coord(x, y - 2 * k)
  }

  private def isVisible(grid: Grid, coord: Coord, fromDirection: Direction): Boolean = {
    val Grid(nrRows, nrCols, heights) = grid
    val Coord(x0, y0) = coord

    def isValid(c: Coord): Boolean =
      0 <= c.x && c.x < nrRows && 0 <= c.y && c.y < nrCols

    val height: Int = heights(x0)(y0)
    height > 0 && Iterator
      .from(1)
      .map(ray(coord, fromDirection))
      .takeWhile(isValid)
      .map { case Coord(x, y) => heights(x)(y) }
      .zipWithIndex
      .forall { case (h, ix) => height >= h - ix }
  }

  def countVisibleCoordinates(grid: Grid): Int = {
    val Grid(nrRows, nrCols, _) = grid
    val coords: List[Coord] = (0 until nrRows).flatMap(x => (0 until nrCols).map(y => Coord(x, y))).toList
    Direction.values.foldLeft(0)((acc, direction) => acc + coords.count(isVisible(grid, _, direction)))
  }

  def main(args: Array[String]): Unit = {
    val lines: Iterator[String] = scala.io.Source.stdin.getLines()
    val grid: Grid = parseInput(lines.toList)
    val result: Int = countVisibleCoordinates(grid)
    println(result)
  }

end Quest08B
