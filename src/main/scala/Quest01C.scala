object Quest01C:
  private def supplementCones(nrCones: Int): Int =
    val k: Int = math.max(3, math.ceil(math.sqrt(nrCones + 1)).toInt)
    if k % 2 == 0 then (k + 1) * (k + 1) - 1 - nrCones
    else k * k - 1 - nrCones

  def getSumOfRequiredCones(pineCones: List[Int]): Int =
    pineCones.map(supplementCones).sum

  def main(args: Array[String]): Unit =
    val line: String = scala.io.StdIn.readLine()
    val pineCones: List[Int] = line.split(",").map(_.toInt).toList
    val result: Int = getSumOfRequiredCones(pineCones)
    println(result)

end Quest01C
