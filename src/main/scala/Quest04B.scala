object Quest04B:
  case class QuestResult(participantId: Int, questId: Int, part: Int, time: Int)
  case class ParticipantResult(participantId: Int, completedParts: Int, time: Int)

  object ParticipantResult:
    given Ordering[ParticipantResult] =
      Ordering.by[ParticipantResult, Int](_.completedParts)(using Ordering.Int.reverse).orElseBy(_.time)

  def parseInput(lines: List[String]): List[QuestResult] =
    def parse(line: String): QuestResult = line match
      case s"id=$id quest=$quest part=$part time=$time" => QuestResult(id.toInt, quest.toInt, part.toInt, time.toInt)
      case _ => throw Exception(s"Malformed input $line.")

    lines.map(parse)

  def calcResultChecksum(questResults: List[QuestResult]): Long = {
    questResults
      .groupMapReduce(_.participantId)(questResult => (1, questResult.time)) {
        case ((np1, t1), (np2, t2)) => (np1 + np2, t1 + t2)
      }
      .map { case (id, (parts, time)) => ParticipantResult(id, parts, time) }
      .toList
      .sorted
      .zipWithIndex
      .foldLeft(0L) { case (acc, (ParticipantResult(_, _, time), ix)) => acc + (ix + 1) * time }
  }

  def main(args: Array[String]): Unit =
    val lines: Iterator[String] = scala.io.Source.stdin.getLines()
    val questResults: List[QuestResult] = parseInput(lines.toList)
    val result: Long = calcResultChecksum(questResults)
    println(result)

end Quest04B
