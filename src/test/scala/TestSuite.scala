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
          "ISO-2341 945"
        )

        val votes: List[Vote] = parseInput(lines)
        calcCandidateFinalScore(votes) shouldEqual 1161463L
      }
    }

    "Part 3" - {
      import Quest03C.{Vote, calcCandidateFinalScore, parseInput}

      "should calculate the candidate's final score when the decimal representation is unique" in {
        val lines: List[String] = List(
          "ISO2-3 23",
          "ISO2-61 149",
          "ISO2-978 99",
          "ISO2-5887 5923",
          "ISO2-76988 123749",
          "ISO2-435657 929977",
          "ISO2-3949 870737"
        )

        val votes: List[Vote] = parseInput(lines)
        calcCandidateFinalScore(votes) shouldEqual 50987L
      }
    }
  }

  "Quest 04" - {
    "Part 1" - {
      import Quest04A.{QuestResult, calcResultChecksum, parseInput}

      "should calculate the checksum of the participant's quest results" in {
        val lines: List[String] = List(
          "id=1 quest=1 part=1 time=187",
          "id=3 quest=1 part=1 time=413",
          "id=5 quest=1 part=3 time=744",
          "id=2 quest=1 part=3 time=745",
          "id=4 quest=1 part=2 time=558",
          "id=1 quest=1 part=2 time=299",
          "id=1 quest=1 part=3 time=362",
          "id=4 quest=1 part=3 time=1268",
          "id=4 quest=1 part=1 time=457",
          "id=5 quest=1 part=1 time=257",
          "id=2 quest=1 part=1 time=192",
          "id=3 quest=1 part=3 time=1168",
          "id=5 quest=1 part=2 time=346",
          "id=3 quest=1 part=2 time=455",
          "id=2 quest=1 part=2 time=562"
        )

        val questResults: List[QuestResult] = parseInput(lines)
        calcResultChecksum(questResults) shouldEqual 27598L
      }
    }

    "Part 2" - {
      import Quest04B.{QuestResult, calcResultChecksum, parseInput}

      "should calculate the checksum of the participant's quest results based on completed quests and total time" in {
        val lines: List[String] = List(
          "id=2 quest=1 part=1 time=100",
          "id=4 quest=2 part=1 time=150",
          "id=1 quest=2 part=1 time=140",
          "id=1 quest=1 part=2 time=180",
          "id=2 quest=2 part=1 time=130",
          "id=4 quest=1 part=1 time=110",
          "id=1 quest=2 part=2 time=210",
          "id=2 quest=2 part=2 time=240",
          "id=1 quest=2 part=3 time=330",
          "id=2 quest=1 part=2 time=170",
          "id=3 quest=1 part=1 time=90",
          "id=4 quest=1 part=2 time=175",
          "id=3 quest=2 part=1 time=120",
          "id=1 quest=1 part=1 time=120",
          "id=3 quest=2 part=2 time=220",
          "id=2 quest=2 part=3 time=350",
          "id=3 quest=1 part=2 time=160"
        )

        val questResults: List[QuestResult] = parseInput(lines)
        calcResultChecksum(questResults) shouldEqual 6470L
      }
    }
  }

  "Quest 05" - {
    "Part 1" - {
      import Quest05A.{Board, parseInput, convertCellsToLand}

      "should calculate the number of land cells" in {
        val lines: List[String] = List(
          "010100100100001101000",
          "111101001000001110000",
          "101111011000101010011",
          "001000101010000100000",
          "000110100110011001100",
          "000001110111111000000",
          "101100010101011110000",
          "001101100000001010011",
          "001001001111010001001",
          "100011011110011010101",
          "001100101110010010000"
        )

        val board: Board = parseInput(lines)
        convertCellsToLand(board) shouldEqual 137
      }
    }
  }
}
