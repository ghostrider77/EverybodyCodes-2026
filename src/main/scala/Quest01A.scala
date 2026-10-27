object Quest01A:
  private def supplementCones(nrCones: Int): Int =
    val k: Int = math.ceil(math.sqrt(nrCones)).toInt
    k * k - nrCones

  def getSumOfRequiredCones(pineCones: List[Int]): Int =
    pineCones.map(supplementCones).sum

  def main(args: Array[String]): Unit =
    val line: String = scala.io.StdIn.readLine()
    val pineCones: List[Int] = line.split(",").map(_.toInt).toList
    val result: Int = getSumOfRequiredCones(pineCones)
    println(result)

end Quest01A
