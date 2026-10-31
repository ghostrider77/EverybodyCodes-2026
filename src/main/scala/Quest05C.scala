object Quest05C:
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

  private def canBeConvertedToLand(cell: Cell, land: Set[Cell], board: Board): Boolean =
    val Board(nrRows, nrCols, darkCells) = board
    getNeighbors(cell, nrRows, nrCols).count(coord => land.contains(coord) || darkCells.contains(coord)) >= 5

  private def canBeConvertedToSand(cell: Cell, sand: Set[Cell], board: Board): Boolean =
    val Board(nrRows, nrCols, _) = board
    getNeighbors(cell, nrRows, nrCols).count(sand.contains) >= 5

  private def canBeConvertedToTree(cell: Cell, sand: Set[Cell], trees: Set[Cell], board: Board): Boolean =
    val Board(nrRows, nrCols, _) = board
    val neighbors: List[Cell] = getNeighbors(cell, nrRows, nrCols)
    neighbors.forall(c => !sand.contains(c)) && neighbors.count(trees.contains) >= 4

  private def isInitialSand(cell: Cell, land: Set[Cell], board: Board): Boolean =
    val Board(nrRows, nrCols, darkCells) = board
    def isAtTheBoundary(coord: Cell): Boolean =
      0 == coord.x || coord.x == nrRows - 1 || 0 == coord.y || coord.y == nrCols - 1

    land.contains(cell) && (isAtTheBoundary(cell) || getNeighbors(cell, nrRows, nrCols).exists(n => !land.contains(n)))

  private def isTree(cell: Cell, land: Set[Cell], board: Board): Boolean =
    val Board(nrRows, nrCols, _) = board
    def isValidForTree(coord: Cell): Boolean =
      2 <= coord.x && coord.x < nrRows - 2 && 2 <= coord.y && coord.y < nrCols - 2

    val Cell(x, y) = cell
    val coords: Set[Cell] =
      (-2 to 2)
        .flatMap(dx => (-2 to 2).map(dy => Cell(x + dx, y + dy)))
        .toSet
        .diff(Set(Cell(x - 2, y - 2), Cell(x - 2, y + 2), Cell(x + 2, y - 2), Cell(x + 2, y + 2)))
    isValidForTree(cell) && coords.forall(land.contains)

  private def convertCellsToLand(board: Board): Set[Cell] =
    val Board(nrRows, nrCols, darkCells) = board

    @tailrec
    def loop(land: Set[Cell]): Set[Cell] =
      val boundaries: Set[Cell] = land.flatMap(getNeighbors(_, nrRows, nrCols)).diff(land)
      val convertibles: Set[Cell] = boundaries.filter(canBeConvertedToLand(_, land, board))
      if convertibles.isEmpty then land
      else loop(land ++ convertibles)

    val startLand = Cell(nrRows / 2, nrCols / 2)
    loop(Set(startLand))

  private def convertCellsToSand(board: Board, land: Set[Cell]): (Set[Cell], Set[Cell]) =
    val Board(nrRows, nrCols, darkCells) = board

    @tailrec
    def loop(land: Set[Cell], sand: Set[Cell]): (Set[Cell], Set[Cell]) =
      val boundaries: Set[Cell] = land.flatMap(getNeighbors(_, nrRows, nrCols)).diff(sand)
      val convertibles: Set[Cell] = boundaries.filter(canBeConvertedToSand(_, sand, board))
      if convertibles.isEmpty then (land, sand)
      else
        loop(land -- convertibles, sand ++ convertibles)

    val initialSand: Set[Cell] = land.filter(isInitialSand(_, land, board))
    loop(land -- initialSand, initialSand)

  private def convertCellsToTrees(board: Board, land: Set[Cell], sand: Set[Cell]): (Set[Cell], Set[Cell]) =
    val Board(nrRows, nrCols, darkCells) = board

    @tailrec
    def loop(currentLand: Set[Cell], trees: Set[Cell], nrRounds: Int): (Set[Cell], Set[Cell]) =
      if nrRounds == 2 then (currentLand, trees)
      else
        val newTrees: Set[Cell] = currentLand.filter(canBeConvertedToTree(_, sand, trees, board))
        loop(currentLand -- newTrees, trees ++ newTrees, nrRounds + 1)

    val initialTrees: Set[Cell] = land.filter(isTree(_, land, board))
    loop(land -- initialTrees, initialTrees, 0)

  def identifyLandWithSandAndTrees(board: Board): Int =
    val land: Set[Cell] = convertCellsToLand(board)
    val (remainingLand: Set[Cell], sand: Set[Cell]) = convertCellsToSand(board, land)
    val (_: Set[Cell], trees: Set[Cell]) = convertCellsToTrees(board, remainingLand, sand)
    trees.size

  def main(args: Array[String]): Unit =
    val lines: Iterator[String] = scala.io.Source.stdin.getLines()
    val board: Board = parseInput(lines.toList)
    val result: Int = identifyLandWithSandAndTrees(board)
    println(result)

end Quest05C
