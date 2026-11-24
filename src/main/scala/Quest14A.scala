object Quest14A:
  case class Coord(x: Int, y: Int)
  case class Window(nrRows: Int, nrCols: Int, pieces: Vector[Vector[Char]])

  def readInput(lines: List[String]): Window = lines match
    case Nil => throw Exception("Empty input.")
    case row :: _ =>
      val nrRows: Int = lines.length
      val nrCols: Int = row.length
      val pieces: Vector[Vector[Char]] = lines.map(_.toVector).toVector
      Window(nrRows, nrCols, pieces)

  def calcBoundingSegmentLength(window: Window): Int = {
    val Window(nrRows, nrCols, pieces) = window

    def isValid(coord: Coord): Boolean =
      0 <= coord.x && coord.x < nrRows && 0 <= coord.y && coord.y < nrCols

    def getValidNeighbors(coord: Coord): List[Coord] = {
      val Coord(x0, y0) = coord
      Iterator(Coord(x0 + 1, y0), Coord(x0, y0 - 1), Coord(x0 - 1, y0), Coord(x0, y0 + 1)).filter(isValid).toList
    }

    def getNrDifferentNeighbors(coord: Coord): Int = {
      val Coord(x0, y0) = coord
      val char: Char = pieces(x0)(y0)
      val neighbors: List[Coord] = getValidNeighbors(coord)
      neighbors.count { case Coord(x, y) => pieces(x)(y) != char }
    }

    val externalBoundaries: Int = 2 * (nrRows + nrCols)
    val innerBoundaries: Int =
      (0 until nrRows)
        .flatMap(x => (0 until nrCols).map(y => Coord(x, y)))
        .foldLeft(0) { case (acc, coord) => acc + getNrDifferentNeighbors(coord) }

    externalBoundaries + innerBoundaries / 2
  }

  def main(args: Array[String]): Unit = {
    val lines: Iterator[String] = scala.io.Source.stdin.getLines()
    val window: Window = readInput(lines.toList)
    val result: Int = calcBoundingSegmentLength(window)
    println(result)
  }

end Quest14A
