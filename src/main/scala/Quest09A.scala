object Quest09A:
  case class Plant(id: Int, growth: Int, limit: Int, branchLengths: List[Int] = List(0), branchingPoints: Int = 0):
    def isLimitReached: Boolean =
      branchLengths.sum + branchingPoints >= limit

    def grow(): Plant = {
      val increasedLengths: List[Int] = branchLengths.map(_ + 1)
      val nrNewBranches: Int = increasedLengths.count(_ == growth)
      val updatedLengths: List[Int] =
        increasedLengths.flatMap(h => if h < growth then List(h) else List(growth / 2, growth / 2, 1))
      this.copy(branchLengths = updatedLengths, branchingPoints = branchingPoints + nrNewBranches)
    }

  def readInput(lines: List[String]): List[Plant] = {
    def parse(line: String): Plant = line match
      case s"id=$id, growth=$growth, limit=$limit" => Plant(id.toInt, growth.toInt, limit.toInt)
      case _ => throw Exception(s"Malformed input $line.")

    lines.map(parse)
  }

  private def checkWhenLimitIsReached(plant: Plant): Int =
    Iterator.iterate(plant)(_.grow()).indexWhere(_.isLimitReached)

  def calcTotalPlantGrowthTime(plants: List[Plant]): Int =
    plants.map(checkWhenLimitIsReached).sum

  def main(args: Array[String]): Unit = {
    val lines: Iterator[String] = scala.io.Source.stdin.getLines()
    val plants: List[Plant] = readInput(lines.toList)
    val result: Int = calcTotalPlantGrowthTime(plants)
    println(result)
  }

end Quest09A
