object Quest02A:
  type OrderedPairs = List[(Int, Int)]

  def parseOrderedPairs(line: String): OrderedPairs =
    def readPair(s: String): (Int, Int) = s match
      case s"$a<$b" => (a.toInt, b.toInt)
      case s"$a>$b" => (b.toInt, a.toInt)
      case _ => throw Exception(s"Malformed input $s.")

    line.split(" ").map(readPair).toList

  private def orderAlloys(pairs: OrderedPairs): List[Int] =
    val alloys: List[Int] = pairs.toSet.flatMap { case (a, b) => Set(a, b) }.toList.sorted
    alloys.sortWith(pairs.contains(_, _))

  def getAlloyGroupChecksum(alloyPairs: List[OrderedPairs]): Long =
    def getGroupNumber(pairs: OrderedPairs): Long = orderAlloys(pairs).reverse.mkString.toLong
    alloyPairs.map(getGroupNumber).sum

  def main(args: Array[String]): Unit =
    val lines: Iterator[String] = scala.io.Source.stdin.getLines()
    val alloyPairs: List[OrderedPairs] = lines.map(parseOrderedPairs).toList
    val result: Long = getAlloyGroupChecksum(alloyPairs)
    println(result)

end Quest02A
