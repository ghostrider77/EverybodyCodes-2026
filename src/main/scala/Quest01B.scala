object Quest01B:
  private def supplementCones(nrCones: Int): Int =
    val k: Int = math.max(3, math.ceil(1 + nrCones / 4.0).toInt)
    4 * k - 4 - nrCones

  def getSumOfRequiredCones(pineCones: List[Int]): Int =
    pineCones.map(supplementCones).sum

  def main(args: Array[String]): Unit =
    val line: String = scala.io.StdIn.readLine()
    val pineCones: List[Int] = line.split(",").map(_.toInt).toList
    val result: Int = getSumOfRequiredCones(pineCones)
    println(result)

end Quest01B
