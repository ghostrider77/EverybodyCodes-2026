object Quest07B:
  import scala.annotation.tailrec

  case class Cloud(id: Int, energy: Int, connections: List[Int])
  case class CloudSystem(adjacencyList: Map[Int, List[Int]], energyLevels: Map[Int, Long])

  object CloudSystem:
    def apply(clouds: List[Cloud]): CloudSystem = {
      val adjacencyList: Map[Int, List[Int]] = clouds.map { case Cloud(id, _, connections) => (id, connections) }.toMap
      val energies: Map[Int, Long] = clouds.map { case Cloud(id, energy, _) => (id, energy.toLong) }.toMap
      new CloudSystem(adjacencyList, energies)
    }

  def readInput(lines: List[String]): CloudSystem = {
    def parse(line: String): Cloud = line match
      case s"id=$id, energy=$energy, connections=$conns" =>
        Cloud(id.toInt, energy.toInt, conns.split(",").map(_.toInt).toList)
      case _ => throw Exception(s"Malformed input $line")

    val clouds: List[Cloud] = lines.map(parse)
    CloudSystem(clouds)
  }

  def sendImpulses(cloudSystem: CloudSystem, initialImpulse: Long): Long = {
    @tailrec
    def loop(clouds: CloudSystem, impulse: Long, targetIds: List[(Int, Int)]): Long = {
      if impulse == 0 then clouds.energyLevels.valuesIterator.sum
      else
        val impulsesToAdd: List[(Int, Long)] = targetIds.map((id, count) => (id, count * impulse))
        val updatedEnergies: Map[Int, Long] = impulsesToAdd.foldLeft(clouds.energyLevels) {
          case (acc, (id, totalImpulse)) => acc.updated(id, totalImpulse + acc.getOrElse(id, 0L))
        }
        val nextTargetIds: List[(Int, Int)] =
          targetIds
            .flatMap((id, count) => clouds.adjacencyList.getOrElse(id, Nil).map(k => (k, count)))
            .groupMapReduce(_._1)(_._2)(_ + _)
            .toList
        loop(clouds.copy(energyLevels = updatedEnergies), impulse - 1, nextTargetIds)
    }

    val startingCloud: Int = cloudSystem.adjacencyList.maxBy { case (_, connections) => connections.length }._1
    loop(cloudSystem, initialImpulse, List((startingCloud, 1)))
  }

  def main(args: Array[String]): Unit = {
    val lines: Iterator[String] = scala.io.Source.stdin.getLines()
    val clouds: CloudSystem = readInput(lines.toList)
    val initialImpulse: Long = 10
    val result: Long = sendImpulses(clouds, initialImpulse)
    println(result)
  }

end Quest07B
