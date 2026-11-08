object Quest09B:
  case class Branch(length: Int, hasLeaf: Boolean)

  case class Plant(id: Int, growth: Int, limit: Int, branches: List[Branch] = List(Branch(0, true))):
    private val nrLeaves: Int = branches.count(branch => branch.length > 0 && branch.hasLeaf)

    def isLeafLimitReached: Boolean = nrLeaves >= limit

    def grow(): Plant = {
      def split(branch: Branch): List[Branch] =
        if branch.length < growth then List(branch)
        else if branch.hasLeaf then List(Branch(growth / 2 , true), Branch(growth / 2, false), Branch(1, true))
        else List(Branch(growth / 2, false), Branch(growth / 2, false), Branch(1 , true))

      val grownBranches: List[Branch] = branches.map(branch => branch.copy(length = branch.length + 1)).flatMap(split)
      this.copy(branches = grownBranches)
    }
  end Plant

  def readInput(lines: List[String]): List[Plant] = {
    def parse(line: String): Plant = line match
      case s"id=$id, growth=$growth, limit=$limit" => Plant(id.toInt, growth.toInt, limit.toInt)
      case _ => throw Exception(s"Malformed input $line.")

    lines.map(parse)
  }

  private def checkWhenLimitIsReached(plant: Plant): Int =
    Iterator.iterate(plant)(_.grow()).indexWhere(_.isLeafLimitReached)

  def calcTotalPlantGrowthTime(plants: List[Plant]): Int =
    plants.map(checkWhenLimitIsReached).sum

  def main(args: Array[String]): Unit = {
    val lines: Iterator[String] = scala.io.Source.stdin.getLines()
    val plants: List[Plant] = readInput(lines.toList)
    val result: Int = calcTotalPlantGrowthTime(plants)
    println(result)
  }

end Quest09B
