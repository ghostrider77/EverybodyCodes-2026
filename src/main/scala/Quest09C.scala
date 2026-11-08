object Quest09C:
  import scala.annotation.tailrec

  case class Branch(length: Long, hasLeaf: Boolean)
  case class Plant(id: Int, growth: Long, limit: Long)

  def readInput(lines: List[String]): List[Plant] = {
    def parse(line: String): Plant = line match
      case s"id=$id, growth=$growth, limit=$limit" => Plant(id.toInt, growth.toLong, limit.toLong)
      case _ => throw Exception(s"Malformed input $line.")

    lines.map(parse)
  }

  private def checkWhenLimitIsReached(plant: Plant): Long = {
    val Plant(_, growth: Long, limit: Long) = plant

    def grow(branch: Branch, step: Long): List[Branch] =
      if branch.length + step < growth then List(branch.copy(length = branch.length + step))
      else List(Branch(growth / 2 , branch.hasLeaf), Branch(growth / 2, false), Branch(1, true))

    @tailrec
    def loop(seconds: Long, branchCounts: Map[Branch, Long]): Long = {
      val nrLeaves: Long = branchCounts.iterator.collect{ case (branch, count) if branch.hasLeaf => count }.sum
      if nrLeaves >= limit then seconds
      else {
        val longestBranchLength: Long = branchCounts.keysIterator.map(_.length).max
        val stepSize: Long = growth - longestBranchLength
        val updatedCounts: Map[Branch, Long] =
          branchCounts
            .toList
            .flatMap{ case (branch, count) => grow(branch, stepSize).map((_, count)) }
            .groupMapReduce(_._1)(_._2)(_ + _)
        loop(seconds + stepSize, updatedCounts)
      }
    }

    loop(1, Map(Branch(1L, true) -> 1))
  }

  def calcTotalPlantGrowthTime(plants: List[Plant]): Long =
    plants.map(checkWhenLimitIsReached).sum

  def main(args: Array[String]): Unit = {
    val lines: Iterator[String] = scala.io.Source.stdin.getLines()
    val plants: List[Plant] = readInput(lines.toList)
    val result: Long = calcTotalPlantGrowthTime(plants)
    println(result)
  }

end Quest09C
