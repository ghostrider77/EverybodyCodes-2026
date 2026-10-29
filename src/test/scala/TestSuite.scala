import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class TestSuite extends AnyFreeSpec, Matchers {
  "Quest 01" - {
    val cones: List[Int] = List(3, 11, 12, 16, 24, 25)

    "Part 1" - {
      import Quest01A.getSumOfRequiredCones

      "should calculate the number of cones needed for a perfect square" in {
        getSumOfRequiredCones(cones) shouldEqual 11
      }
    }

    "Part 2" - {
      import Quest01B.getSumOfRequiredCones

      "should calculate the number of cones needed for a square frame" in {
        getSumOfRequiredCones(cones) shouldEqual 9
      }
    }

    "Part 3" - {
      import Quest01C.getSumOfRequiredCones

      "should calculate the number of cones needed for an almost filled square" in {
        getSumOfRequiredCones(cones) shouldEqual 61
      }
    }
  }

  "Quest 02" - {
    "Part 1" - {
      import Quest02A.{OrderedPairs, getAlloyGroupChecksum, parseOrderedPairs}

      "should calculate the sum of the ordered alloys given the total ordering" in {
        val lines: Iterator[String] = Iterator(
          "4<3 2<5 3>1 1>2 1<5 4<1 3>2 4<2 3>5 4<5",
          "4>1 2>1 3>2 1<5 4>5 2<5 3>1 4>3 2<4 5<3",
          "5>1 5>2 4>1 2<4 3>2 3<4 1>2 3>5 1<3 4>5"
        )
        val alloyPairs: List[OrderedPairs] = lines.map(parseOrderedPairs).toList
        getAlloyGroupChecksum(alloyPairs) shouldEqual 122157L
      }
    }

    "Part 2" - {
      import Quest02B.{OrderedPairs, getAlloyGroupChecksum, parseOrderedPairs}

      "should calculate the sum of the ordered alloys given the almost total ordering" in {
        val lines: Iterator[String] = Iterator(
          "2<5 3>1 1>2 1<5 4<1 3>2 4<2 3>5 4<5",
          "2>1 3>2 1<5 4>5 2<5 3>1 4>3 2<4 5<3",
          "5>1 5>2 4>1 3>2 3<4 1>2 3>5 1<3 4>5"
        )
        val alloyPairs: List[OrderedPairs] = lines.map(parseOrderedPairs).toList
        getAlloyGroupChecksum(alloyPairs) shouldEqual 122157L
      }
    }

    "Part 3" - {
      import Quest02C.{Graph, getAlloyGroupChecksum, readGraph}

      "should calculate the sum of the ordered alloys given an ordering graph" in {
        val lines: Iterator[String] = Iterator(
          "3>1 1>2 1<5 4<2 3>5",
          "2>1 2<5 3>1 4>3 2<4 5<3",
          "5>1 3<4 1>2 3>5"
        )
        val graphs: List[Graph] = lines.map(readGraph).toList
        getAlloyGroupChecksum(graphs) shouldEqual 122157L
      }
    }
  }

  "Quest 03" - {
    "Part 1" - {
      import Quest03A.{Vote, calcCandidateFinalScore, parseInput}

      "should calculate the candidate's final score in decimal number" in {
        val lines: List[String] = List("ISO-3210 7654", "ISO-8742 2468", "ISO-1111 1111")

        val votes: List[Vote] = parseInput(lines)
        calcCandidateFinalScore(votes) shouldEqual 1478
      }
    }

    "Part 2" - {
      import Quest03B.{Vote, calcCandidateFinalScore, parseInput}

      "should calculate the candidate's final score in decimal number when the parts can have different lengths" in {
        val lines: List[String] = List(
          "ISO-3210 7654",
          "ISO-8742 2468",
          "ISO-1 1111",
          "ISO-13 777",
          "ISO-34 140856",
          "ISO-789 2465112",
          "ISO-2341 945",
        )

        val votes: List[Vote] = parseInput(lines)
        calcCandidateFinalScore(votes) shouldEqual 1161463L
      }
    }
  }
}
