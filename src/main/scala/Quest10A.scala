object Quest10A:
  import scala.annotation.tailrec
  import Direction.*

  case class Coord(x: Int, y: Int)

  enum Direction:
    case North, East, South, West

  object Direction:
    def apply(char: Char): Direction = char match
      case '>' => East
      case 'v' => South
      case '<' => West
      case '^' => North
      case _ => throw Exception(s"Unknown direction $char.")

  case class NullGhost(position: Coord, direction: Direction, wallCollisions: Int = 0)

  def readInput(lines: List[String]): (List[NullGhost], Set[Coord]) = lines match
    case Nil => throw Exception("Empty input.")
    case _ =>
      val cells: List[(Coord, Char)] =
        lines
          .zipWithIndex
          .flatMap { case (line, x) => line.toList.zipWithIndex.map {case (c, y) => (Coord(x, y), c)} }
      val walls: Set[Coord] = cells.collect{ case (coord, char) if char == '#' => coord }.toSet
      val nullGhosts: List[NullGhost] =
        cells.collect { case (coord, char) if char != '#' && char != '.' => NullGhost(coord, Direction(char)) }
      (nullGhosts, walls)

  private def changeDirection(direction: Direction): Direction = direction match
    case North => East
    case East => South
    case South => West
    case West => North

  private def move(nullGhost: NullGhost, walls: Set[Coord]): NullGhost = {
    val NullGhost(position @ Coord(x, y), direction, wallCollisions) = nullGhost
    val nextPosition: Coord = direction match
      case North => Coord(x - 1, y)
      case East => Coord(x, y + 1)
      case South => Coord(x + 1, y)
      case West => Coord(x, y - 1)
    if walls.contains(nextPosition) then NullGhost(position, changeDirection(direction), wallCollisions + 1)
    else nullGhost.copy(position = nextPosition)
  }

  private def nullGhostCollisions(nullGhosts: List[NullGhost]): (List[NullGhost], Int) = {
    val collisions: Map[Coord, Int] =
      nullGhosts
      .groupMapReduce(_.position)(_ => 1)(_ + _)
      .filter((_, count) => count >= 2)
    val nextGhosts: List[NullGhost] =
      nullGhosts.map {
        case nullGhost @ NullGhost(coord, direction, _) =>
          if collisions.contains(coord) then nullGhost.copy(direction = changeDirection(changeDirection(direction)))
          else nullGhost
      }
    (nextGhosts, collisions.valuesIterator.sum)
  }

  def simulate(walls: Set[Coord], nullGhosts: List[NullGhost], nrRounds: Int): Int = {
    @tailrec
    def loop(acc: Int, currentGhosts: List[NullGhost], k: Int): Int =
      if k == nrRounds then acc + currentGhosts.map(_.wallCollisions).sum
      else {
        val ghostsAfterMove: List[NullGhost] = currentGhosts.map(move(_, walls))
        val (nextGhosts: List[NullGhost], ghostCollisions: Int) = nullGhostCollisions(ghostsAfterMove)
        loop(acc + ghostCollisions, nextGhosts, k + 1)
      }

    loop(0, nullGhosts, 0)
  }

  def main(args: Array[String]): Unit = {
    val lines: Iterator[String] = scala.io.Source.stdin.getLines()
    val (nullGhosts: List[NullGhost], caveWalls: Set[Coord]) = readInput(lines.toList)
    val nrRounds: Int = 10
    val result: Int = simulate(caveWalls, nullGhosts, nrRounds)
    println(result)
  }

end Quest10A
