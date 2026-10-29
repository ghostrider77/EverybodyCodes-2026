object Quest03A:
  case class Vote(standard: Long, vote: Long):
    def toLong: Long =
      val weights: List[Long] =
        standard
          .toString
          .map(_.asDigit)
          .reverseIterator
          .zipWithIndex.map { case (d, ix) => math.pow(d, ix).toLong }
          .toList
          .reverse
      vote.toString.map(_.asDigit).zip(weights).foldLeft(0L) { case (acc, (d, w)) => acc + d * w }

  object Vote:
    def apply(s: String): Vote = s match
      case s"ISO-$standard $vote" => Vote(standard.toLong, vote.toLong)
      case _ => throw Exception(s"Malformed input $s.")

  def parseInput(lines: List[String]): List[Vote] =
    lines.map(Vote(_))

  def calcCandidateFinalScore(votes: List[Vote]): Long =
    votes.map(_.toLong).sum

  def main(args: Array[String]): Unit =
    val lines: Iterator[String] = scala.io.Source.stdin.getLines()
    val votes: List[Vote] = parseInput(lines.toList)
    val result: Long = calcCandidateFinalScore(votes)
    println(result)

end Quest03A
