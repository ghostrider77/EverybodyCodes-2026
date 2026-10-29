object Quest03C:
  case class Vote(standard: Long, vote: Long):
    private def toLong(voteNumber: Long): Long =
      val voteDigits: List[Int] = voteNumber.toString.map(_.asDigit).toList
      val weights: List[Long] =
        Iterator
          .continually(standard.toString.map(_.asDigit).reverse)
          .flatten
          .take(voteDigits.length)
          .zipWithIndex.map { case (d, ix) => math.pow(d, ix).toLong }
          .toList
          .reverse
      voteDigits.zip(weights).foldLeft(0L) { case (acc, (d, w)) => acc + d * w }

    private lazy val isValid: Boolean =
      val decimalValue: Long = toLong(vote)
      (vote - 1 to 0 by -1).forall(v => toLong(v) != decimalValue)

    lazy val decimalVote: Long =
      if isValid then toLong(vote) else 0L

  object Vote:
    def apply(s: String): Vote = s match
      case s"ISO2-$standard $vote" => Vote(standard.toLong, vote.toLong)
      case _ => throw Exception(s"Malformed input $s.")

  def parseInput(lines: List[String]): List[Vote] =
    lines.map(Vote(_))

  def calcCandidateFinalScore(votes: List[Vote]): Long =
    votes.map(_.decimalVote).sum

  def main(args: Array[String]): Unit =
    val lines: Iterator[String] = scala.io.Source.stdin.getLines()
    val votes: List[Vote] = parseInput(lines.toList)
    val result: Long = calcCandidateFinalScore(votes)
    println(result)

end Quest03C
