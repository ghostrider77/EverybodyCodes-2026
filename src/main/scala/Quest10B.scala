object Quest10B:
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

  case class NullGhost(position: Coord, direction: Direction, collisions: Int = 0)

  def readInput(lines: List[String]): (List[NullGhost], Set[Coord]) = lines match
    case Nil => throw Exception("Empty input.")
    case _ =>
      val cells: List[(Coord, Char)] =
        lines
          .zipWithIndex
          .flatMap { case (line, x) => line.toList.zipWithIndex.map { case (c, y) => (Coord(x, y), c) } }
      val walls: Set[Coord] = cells.collect { case (coord, char) if char == '#' => coord }.toSet
      val nullGhosts: List[NullGhost] =
        cells.collect { case (coord, char) if char != '#' && char != '.' => NullGhost(coord, Direction(char)) }
      (nullGhosts, walls)

  private def changeDirection(direction: Direction): Direction = direction match
    case North => East
    case East => South
    case South => West
    case West => North

  private def move(nullGhost: NullGhost, walls: Set[Coord]): NullGhost = {
    val NullGhost(position@Coord(x, y), direction, wallCollisions) = nullGhost
    val nextPosition: Coord = direction match
      case North => Coord(x - 1, y)
      case East => Coord(x, y + 1)
      case South => Coord(x + 1, y)
      case West => Coord(x, y - 1)
    if walls.contains(nextPosition) then NullGhost(position, changeDirection(direction), wallCollisions + 1)
    else nullGhost.copy(position = nextPosition)
  }

  private def nullGhostCollisions(nullGhosts: List[NullGhost]): List[NullGhost] = {
    val ghostCollisions: Map[Coord, Int] =
      nullGhosts
        .groupMapReduce(_.position)(_ => 1)(_ + _)
        .filter((_, count) => count >= 2)
    val nextGhosts: List[NullGhost] =
      nullGhosts.map {
        case nullGhost@NullGhost(coord, direction, collisions) =>
          if ghostCollisions.contains(coord) then {
            val newDirection: Direction = changeDirection(changeDirection(direction))
            nullGhost.copy(direction = newDirection, collisions = collisions + 1)
          } else nullGhost
      }
    nextGhosts
  }

  def simulate(walls: Set[Coord], nullGhosts: List[NullGhost], lifespan: Int): Int = {
    @tailrec
    def loop(currentGhosts: List[NullGhost], k: Int): Int =
      if currentGhosts.isEmpty then k
      else
        val ghostsAfterMove: List[NullGhost] = currentGhosts.map(move(_, walls))
        val nextGhosts: List[NullGhost] = nullGhostCollisions(ghostsAfterMove).filter(_.collisions < lifespan)
        loop(nextGhosts, k + 1)

    loop(nullGhosts, 0)
  }

  def main(args: Array[String]): Unit = {
    val lines: Iterator[String] = scala.io.Source.stdin.getLines()
    val (nullGhosts: List[NullGhost], caveWalls: Set[Coord]) = readInput(lines.toList)
    val lifespan: Int = 10000
    val result: Int = simulate(caveWalls, nullGhosts, lifespan)
    println(result)
  }

end Quest10B
