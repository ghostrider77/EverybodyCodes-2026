object Quest04C:
  import scala.annotation.tailrec
  import Part.{Part1, Part2, Part3}

  enum Part:
    case Part1, Part2, Part3

  private object Part:
    given Ordering[Part] = Ordering.by(_.ordinal)

    def apply(s: String): Part = s match
      case "1" => Part1
      case "2" => Part2
      case "3" => Part3

    def highestScore(part: Part): Int = part match
      case Part1 => 50
      case Part2 => 100
      case Part3 => 150

  case class QuestPartResult(participantId: Int, questId: Int, part: Part, time: Int)
  case class QuestResult(
      participantId: Int,
      afterPart1: Int,
      isPart2Completed: Boolean,
      afterPart2: Int,
      isPart3Completed: Boolean,
      afterPart3: Int
  )

  def parseInput(lines: List[String]): List[QuestPartResult] =
    def parse(line: String): QuestPartResult = line match
      case s"id=$id quest=$quest part=$part time=$time" =>
        QuestPartResult(id.toInt, quest.toInt, Part(part), time.toInt)
      case _ => throw Exception(s"Malformed input $line.")

    lines.map(parse)

  private def addMaps[K](m1: Map[K, Int], m2: Map[K, Int]): Map[K, Int] =
    m2.foldLeft(m1) { case (acc, (key, value)) => acc.updated(key, acc.getOrElse(key, 0) + value) }

  private def calcQuestCompletionTimes(questResults: List[QuestPartResult]): Map[Int, List[QuestResult]] =
    def getCumulativeTimes(xs: List[QuestPartResult]): QuestResult =
      xs.sortBy(_.part).map { case QuestPartResult(participantId, _, _, time) => (participantId, time) } match
        case List((participantId, t1), (_, t2), (_, t3)) =>
          QuestResult(participantId, t1, true, t1 + t2, true, t1 + t2 + t3)
        case List((participantId, t1), (_, t2)) => QuestResult(participantId, t1, true, t1 + t2, false, t1 + t2)
        case List((participantId, t1)) => QuestResult(participantId, t1, false, t1, false, t1)
        case _ => throw Exception("For each participant 1, 2, or 3 quest results are expected.")

    questResults
      .groupBy(_.questId)
      .view
      .mapValues(_.groupBy(_.participantId).map((_, xs) => getCumulativeTimes(xs)).toList)
      .toMap

  private def scoreQuestPart(questResults: List[QuestResult], part: Part): Map[Int, Int] =
    @tailrec
    def loop(acc: Map[Int, Int], rs: List[(Int, Int)], prevTime: Int, previousScore: Int, diff: Int): Map[Int, Int] =
      rs match
        case Nil => acc
        case (pId, time) :: rest =>
          if time == prevTime then loop(acc + (pId -> previousScore), rest, time, previousScore, diff + 1)
          else
            val nextScore: Int = math.max(0, previousScore - diff)
            loop(acc + (pId -> nextScore), rest, time, nextScore, 1)

    val sortedResults: List[(Int, Int)] = part match
      case Part1 => questResults.sortBy(_.afterPart1).map(r => (r.participantId, r.afterPart1))
      case Part2 =>
        questResults.filter(_.isPart2Completed).sortBy(_.afterPart2).map(r => (r.participantId, r.afterPart2))
      case Part3 =>
        questResults.filter(_.isPart3Completed).sortBy(_.afterPart3).map(r => (r.participantId, r.afterPart3))
    loop(Map(), sortedResults, Int.MinValue, Part.highestScore(part), 0)

  private def scoreQuest(questResults: List[QuestResult]): Map[Int, Int] =
    Part.values.foldLeft(Map.empty[Int, Int])((acc, part) => addMaps(acc, scoreQuestPart(questResults, part)))

  private def calcChecksumWithTies(scores: List[Int]): Long =
    val sortedScores: List[Int] = scores.sorted(using Ordering[Int].reverse)

    @tailrec
    def loop(acc: Long, xs: List[Int], prevScore: Int, prevMultiplier: Int, diff: Int): Long = xs match
      case Nil => acc
      case score :: rest =>
        if score == prevScore then loop(acc + prevMultiplier * score, rest, score, prevMultiplier, diff + 1)
        else
          val nextMultiplier: Int = prevMultiplier + diff
          loop(acc + nextMultiplier * score, rest, score, nextMultiplier, 1)

    loop(0L, sortedScores, Int.MinValue, 1, 0)

  def calcResultChecksum(questPartResults: List[QuestPartResult]): Long =
    val participantScores: Map[Int, Int] = questPartResults.map(_.participantId).map(id => (id, 0)).toMap
    val questCompletionTimes: Map[Int, List[QuestResult]] = calcQuestCompletionTimes(questPartResults)
    val scores: Map[Int, Int] =
      questCompletionTimes.values.foldLeft(participantScores)((acc, qrs) => addMaps(acc, scoreQuest(qrs)))

    calcChecksumWithTies(scores.values.toList)

  def main(args: Array[String]): Unit =
    val lines: Iterator[String] = scala.io.Source.stdin.getLines()
    val questPartResults: List[QuestPartResult] = parseInput(lines.toList)
    val result: Long = calcResultChecksum(questPartResults)
    println(result)

end Quest04C
