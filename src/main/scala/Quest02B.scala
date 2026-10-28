object Quest02B:
  type OrderedPairs = List[(Int, Int)]

  def parseOrderedPairs(line: String): OrderedPairs =
    def readPair(s: String): (Int, Int) = s match
      case s"$a<$b" => (a.toInt, b.toInt)
      case s"$a>$b" => (b.toInt, a.toInt)
      case _ => throw Exception(s"Malformed input $s.")

    line.split(" ").map(readPair).toList

  private def orderAlloys(pairs: OrderedPairs): List[Int] =
    val alloys: List[Int] = pairs.toSet.flatMap { case (a, b) => Set(a, b) }.toList.sorted
    def lessThan(a: Int, b: Int): Boolean =
      pairs.contains((a, b)) || alloys.exists(c => pairs.contains((a, c)) && pairs.contains((c, b)))

    alloys.sortWith(lessThan)

  def getAlloyGroupChecksum(alloyPairs: List[OrderedPairs]): Long =
    def getGroupNumber(pairs: OrderedPairs): Long = orderAlloys(pairs).reverse.mkString.toLong

    alloyPairs.map(getGroupNumber).sum

  def main(args: Array[String]): Unit =
    val lines: Iterator[String] = scala.io.Source.stdin.getLines()
    val alloyPairs: List[OrderedPairs] = lines.map(parseOrderedPairs).toList
    val result: Long = getAlloyGroupChecksum(alloyPairs)
    println(result)

end Quest02B
