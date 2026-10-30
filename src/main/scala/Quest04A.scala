object Quest04A:
  case class QuestResult(participantId: Int, questId: Int, part: Int, time: Int)

  def parseInput(lines: List[String]): List[QuestResult] =
    def parse(line: String): QuestResult = line match
      case s"id=$id quest=$quest part=$part time=$time" => QuestResult(id.toInt, quest.toInt, part.toInt, time.toInt)
      case _ => throw Exception(s"Malformed input $line.")

    lines.map(parse)

  def calcResultChecksum(questResults: List[QuestResult]): Long =
    questResults
      .groupMapReduce(_.participantId)(_.time)(_ + _)
      .values
      .toList
      .sorted
      .zipWithIndex
      .foldLeft(0L) { case (acc, (time, ix)) => acc + (ix + 1) * time }

  def main(args: Array[String]): Unit =
    val lines: Iterator[String] = scala.io.Source.stdin.getLines()
    val questResults: List[QuestResult] = parseInput(lines.toList)
    val result: Long = calcResultChecksum(questResults)
    println(result)

end Quest04A
