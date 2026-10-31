object Quest05A:
  import scala.annotation.tailrec

  case class Cell(x: Int, y: Int)
  case class Board(nrRows: Int, nrCols: Int, darkCells: Set[Cell])

  def parseInput(lines: List[String]): Board = lines match
    case head :: _ =>
      val nrRows: Int = lines.length
      val nrCols: Int = head.length
      val darkCells: Set[Cell] =
        lines
          .zipWithIndex
          .flatMap { case (row, x) => row.zipWithIndex.collect { case (p, y) if p == '1' => Cell(x, y) } }
          .toSet
      Board(nrRows, nrCols, darkCells)
    case _ => throw Exception("Empty input.")

  private def getNeighbors(cell: Cell, nrRows: Int, nrCols: Int): List[Cell] =
    def isValid(coord: Cell): Boolean =
      0 <= coord.x && coord.x < nrRows && 0 <= coord.y && coord.y < nrCols

    val Cell(x, y) = cell
    Iterator(
      Cell(x - 1, y - 1),
      Cell(x - 1, y),
      Cell(x - 1, y + 1),
      Cell(x, y - 1),
      Cell(x, y + 1),
      Cell(x + 1, y - 1),
      Cell(x + 1, y),
      Cell(x + 1, y + 1)
    ).filter(isValid).toList

  private def isConvertible(cell: Cell, land: Set[Cell], board: Board): Boolean =
    val Board(nrRows, nrCols, darkCells) = board
    getNeighbors(cell, nrRows, nrCols).count(coord => land.contains(coord) || darkCells.contains(coord)) >= 5

  def convertCellsToLand(board: Board): Int =
    val Board(nrRows, nrCols, darkCells) = board
    @tailrec
    def loop(land: Set[Cell]): Int =
      val boundaries: Set[Cell] = land.flatMap(getNeighbors(_, nrRows, nrCols)).diff(land)
      val convertibles: Set[Cell] = boundaries.filter(isConvertible(_, land, board))
      if convertibles.isEmpty then land.size
      else loop(land ++ convertibles)

    val startLand = Cell(nrRows / 2, nrCols / 2)
    loop(Set(startLand))

  def main(args: Array[String]): Unit =
    val lines: Iterator[String] = scala.io.Source.stdin.getLines()
    val board: Board = parseInput(lines.toList)
    val result: Int = convertCellsToLand(board)
    println(result)

end Quest05A
