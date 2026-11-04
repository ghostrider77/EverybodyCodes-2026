object Quest07A:
  import scala.annotation.tailrec

  case class Cloud(id: Int, energy: Int, connections: List[Int])
  case class CloudSystem(adjacencyList: Map[Int, List[Int]], energyLevels: Map[Int, Int])

  object CloudSystem:
    def apply(clouds: List[Cloud]): CloudSystem = {
      val adjacencyList: Map[Int, List[Int]] = clouds.map { case Cloud(id, _, connections) => (id, connections) }.toMap
      val energies: Map[Int, Int] = clouds.map { case Cloud(id, energy, _) => (id, energy) }.toMap
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

  def sendImpulses(cloudSystem: CloudSystem, initialImpulse: Int): Int = {
    @tailrec
    def loop(clouds: CloudSystem, impulse: Int, targetIds: List[Int]): Int = {
      if impulse == 0 then clouds.energyLevels.valuesIterator.sum
      else
        val impulsesToAdd: Map[Int, Int] = targetIds.groupMapReduce(identity)(_ => impulse)(_ + _)
        val updatedEnergies: Map[Int, Int] = impulsesToAdd.foldLeft(clouds.energyLevels) {
          case (acc, (id, totalImpulse)) => acc.updated(id, totalImpulse + acc.getOrElse(id, 0))
        }
        val nextTargetIds: List[Int] = targetIds.flatMap(clouds.adjacencyList.getOrElse(_, Nil))
        loop(clouds.copy(energyLevels = updatedEnergies), impulse - 1, nextTargetIds)
    }

    val startingCloud: Int = cloudSystem.adjacencyList.maxBy { case (_, connections) => connections.length }._1
    loop(cloudSystem, initialImpulse, List(startingCloud))
  }

  def main(args: Array[String]): Unit = {
    val lines: Iterator[String] = scala.io.Source.stdin.getLines()
    val clouds: CloudSystem = readInput(lines.toList)
    val initialImpulse: Int = 5
    val result: Int = sendImpulses(clouds, initialImpulse)
    println(result)
  }

end Quest07A
