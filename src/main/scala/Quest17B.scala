object Quest17B:
  case class Cell(x: Int, y: Int)

  def readInput(lines: List[String]): Set[Cell] = lines match
    case Nil => throw Exception("Empty input.")
    case line :: _ =>
      lines
        .zipWithIndex
        .flatMap { case (row, x) => row.zipWithIndex.collect { case (char, y) if char == '#' => Cell(x, y) } }
        .toSet

  private def getNrExposedEdges(cell: Cell, squares: Set[Cell]): Int =
    if squares.contains(cell) then {
      val Cell(x, y) = cell
      Iterator(Cell(x - 1, y), Cell(x, y + 1), Cell(x + 1, y), Cell(x, y - 1)).filterNot(squares.contains).length
    } else 0

  def calcLoopLength(solidSquares: Set[Cell]): Int =
    solidSquares.foldLeft(0)((acc, cell) => acc + getNrExposedEdges(cell, solidSquares))

  def main(args: Array[String]): Unit = {
    val lines: Iterator[String] = scala.io.Source.stdin.getLines()
    val solidSquares: Set[Cell] = readInput(lines.toList)
    val result: Int = calcLoopLength(solidSquares)
    println(result)
  }

end Quest17B
