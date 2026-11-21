object Quest19A:
  enum Color:
    private case Red, Green, Blue, Cyan, Magenta, Yellow, Black

    def +(c: Color): Color = (this, c) match
      case (_, _) if this == c => this
      case (Black, _) => Black
      case (_, Black) => Black
      case (Cyan, Magenta) => Blue
      case (Magenta, Cyan) => Blue
      case (Cyan, Yellow) => Green
      case (Yellow, Cyan) => Green
      case (Magenta, Yellow) => Red
      case (Yellow, Magenta) => Red
      case _ => throw Exception("Undefined color mixing.")

  object Color:
    def apply(c: Char): Color = c match
      case 'R' => Red
      case 'G' => Green
      case 'B' => Blue
      case 'C' => Cyan
      case 'M' => Magenta
      case 'Y' => Yellow
      case 'K' => Black
      case _ => throw Exception(s"Unknown color $c.")

  def readInput(lines: List[String]): (List[Color], List[Color]) = {
    def parseColors(row: String): List[Color] =
      row.toList.map(Color(_))

    val (xs: List[String], ys: List[String]) = lines.span(_.nonEmpty)
    (xs.flatMap(parseColors), ys.drop(1).flatMap(parseColors))
  }

  def mostCommonColorProduct(layer1: List[Color], layer2: List[Color]): Long = {
    val overlay: List[Color] = layer1.zip(layer2).map(_ + _)
    overlay
      .groupMapReduce(identity)(_ => 1L)(_ + _)
      .values
      .toList
      .sorted(using Ordering[Long].reverse)
      .take(3)
      .product
  }

  def main(args: Array[String]): Unit = {
    val lines: Iterator[String] = scala.io.Source.stdin.getLines()
    val (layer1: List[Color], layer2: List[Color]) = readInput(lines.toList)
    val result: Long = mostCommonColorProduct(layer1, layer2)
    println(result)
  }

end Quest19A
