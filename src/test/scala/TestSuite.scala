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

    "Part 3" - {
      import Quest04C.{QuestPartResult, calcResultChecksum, parseInput}

      "Test case 1: should calculate the checksum of the participant's results considering potential ties" in {
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

        val questPartResults: List[QuestPartResult] = parseInput(lines)
        calcResultChecksum(questPartResults) shouldEqual 2998L
      }

      "Test case 2: should calculate the checksum of the participant's results considering potential ties" in {
        val lines: List[String] = List(
          "id=2 quest=1 part=1 time=90",
          "id=4 quest=2 part=1 time=150",
          "id=1 quest=2 part=1 time=130",
          "id=1 quest=1 part=2 time=180",
          "id=2 quest=2 part=1 time=130",
          "id=4 quest=1 part=1 time=110",
          "id=1 quest=2 part=2 time=210",
          "id=2 quest=2 part=2 time=220",
          "id=1 quest=2 part=3 time=330",
          "id=2 quest=1 part=2 time=170",
          "id=3 quest=1 part=1 time=90",
          "id=4 quest=1 part=2 time=170",
          "id=3 quest=2 part=1 time=120",
          "id=1 quest=1 part=1 time=120",
          "id=3 quest=2 part=2 time=220",
          "id=2 quest=2 part=3 time=350",
          "id=3 quest=1 part=2 time=170"
        )

        val questPartResults: List[QuestPartResult] = parseInput(lines)
        calcResultChecksum(questPartResults) shouldEqual 3004L
      }
    }
  }

  "Quest 05" - {
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

    "Part 1" - {
      import Quest05A.{Board, parseInput, convertCellsToLand}

      "should calculate the number of land cells" in {
        val board: Board = parseInput(lines)
        convertCellsToLand(board) shouldEqual 137
      }
    }

    "Part 2" - {
      import Quest05B.{Board, parseInput, identifyLandWithSand}

      "should calculate the number of sand cells" in {
        val board: Board = parseInput(lines)
        identifyLandWithSand(board) shouldEqual 69
      }
    }

    "Part 3" - {
      import Quest05C.{Board, parseInput, identifyLandWithSandAndTrees}

      "should calculate the number of tree cells" in {
        val board: Board = parseInput(lines)
        identifyLandWithSandAndTrees(board) shouldEqual 14
      }
    }
  }

  "Quest 06" - {
    val lines: List[String] = List(
      "BRDS:FLWR,SLTT,YSTS",
      "FLWR:SLTT,YSTS",
      "SLTT:YSTS,MLKK,BTTR",
      "NULL:BRDS,FLWR,SLTT",
      "YSTS:BRDS,FLWR",
      "MLKK:SPCS,NULL",
      "HNYN:BRDS,FLWR,SLTT,YSTS"
    )

    "Part 1" - {
      import Quest06A.{DirectedGraph, getEveryIngredientsNeeded, readGraph}

      "should calculate the number of ingredients needed for NULL" in {
        val graph: DirectedGraph = readGraph(lines)
        getEveryIngredientsNeeded(graph) shouldEqual 7
      }
    }

    "Part 2" - {
      import Quest06B.{DirectedGraph, Edge, getEveryIngredientsNeeded, readEdges}

      "should calculate the number of ingredients that produces NULL" in {
        val edges: List[Edge] = readEdges(lines)
        val graph = DirectedGraph(edges)
        getEveryIngredientsNeeded(graph) shouldEqual 6
      }
    }

    "Part 3" - {
      import Quest06C.{DirectedGraph, Edge, getIngredientsUserAtLeast2Nulls, readEdges}

      val lines: List[String] = List(
        "MEAL:BRDS,CAKE,DOUG",
        "BRDS:DOUG,FLWR",
        "CAKE:DOUG,FLWR,NULLv2,NULLv3",
        "DOUG:FLWR,YSTS,SLTT,NULLv1",
        "FLWR:BRDS,DOUG",
        "YSTS:SPCS",
        "SOUP:BRDS,CAKE,DOUG,FLWR"
      )

      "should calculate the number of ingredients that produces NULL" in {
        val edges: List[Edge] = readEdges(lines)
        val graph = DirectedGraph(edges)
        getIngredientsUserAtLeast2Nulls(graph) shouldEqual 3
      }
    }
  }

  "Quest 07" - {
    "Part 1" - {
      import Quest07A.{CloudSystem, readInput, sendImpulses}

      "should calculate the total impulses the clouds received" in {
        val lines: List[String] = List(
          "id=1, energy=5, connections=2,5,7",
          "id=2, energy=9, connections=6,7",
          "id=3, energy=3, connections=4",
          "id=4, energy=8, connections=3",
          "id=5, energy=1, connections=6,7",
          "id=6, energy=7, connections=1,7",
          "id=7, energy=6, connections=5"
        )
        val clouds: CloudSystem = readInput(lines)
        val initialImpulse: Int = 5
        sendImpulses(clouds, initialImpulse) shouldEqual 102
      }
    }

    "Part 2" - {
      import Quest07B.{CloudSystem, readInput, sendImpulses}

      "Test case 1 - should calculate the total impulses the clouds received" in {
        val lines: List[String] = List(
          "id=1, energy=5, connections=2,5,7",
          "id=2, energy=9, connections=6,7",
          "id=3, energy=3, connections=4",
          "id=4, energy=8, connections=3",
          "id=5, energy=1, connections=6,7",
          "id=6, energy=7, connections=1,7",
          "id=7, energy=6, connections=5"
        )
        val clouds: CloudSystem = readInput(lines)
        val initialImpulse: Int = 10
        sendImpulses(clouds, initialImpulse) shouldEqual 1316
      }

      "Test case 2 - should calculate the total impulses the clouds received" in {
        val lines: List[String] = List(
          "id=1, energy=5, connections=2,3,5,7",
          "id=2, energy=9, connections=1,3,4,5,6,7",
          "id=3, energy=3, connections=1,2,4,6,7",
          "id=4, energy=8, connections=1,2,7",
          "id=5, energy=1, connections=2,3,4,6",
          "id=6, energy=7, connections=1,2,3,4",
          "id=7, energy=6, connections=1,2,4,6"
        )
        val clouds: CloudSystem = readInput(lines)
        val initialImpulse: Int = 10
        sendImpulses(clouds, initialImpulse) shouldEqual 1214633
      }
    }
  }

  "Quest 08" - {
    "Part 1" - {
      import Quest08A.{Grid, countVisibleCoordinates, parseInput}

      "should calculate the number of units of energy required for a round" in {
        val lines: List[String] = List(
          "....1.9.6.1.7....",
          "...9.2.3.3.8.1...",
          "..7.8.3.5.1.4.3..",
          ".4.6.7.4.1.1.8.9.",
          "4.5.3.6.5.8.9.4.9",
          ".9.5.1.5.6.2.8.9.",
          "..7.7.6.4.6.2.4..",
          "...9.3.8.3.8.4...",
          "....1.6.6.2.8...."
        )
        val grid: Grid = parseInput(lines)
        countVisibleCoordinates(grid) shouldEqual 32
      }
    }
  }

  "Quest 09" - {
    val lines: List[String] = List(
      "id=1, growth=3, limit=10",
      "id=2, growth=5, limit=20",
      "id=3, growth=7, limit=30"
    )

    "Part 1" - {
      import Quest09A.{Plant, readInput, calcTotalPlantGrowthTime}

      "should calculate the total time of plants overgrow their limits" in {
        val plants: List[Plant] = readInput(lines)
        calcTotalPlantGrowthTime(plants) shouldEqual 27
      }
    }

    "Part 2" - {
      import Quest09B.{Plant, readInput, calcTotalPlantGrowthTime}

      "should calculate the total time when the number of leaves exceed their limits" in {
        val plants: List[Plant] = readInput(lines)
        calcTotalPlantGrowthTime(plants) shouldEqual 42
      }
    }

    "Part 3" - {
      import Quest09C.{Plant, readInput, calcTotalPlantGrowthTime}

      "should calculate the total time when the number of leaves exceed their limits" in {
        val plants: List[Plant] = readInput(lines)
        calcTotalPlantGrowthTime(plants) shouldEqual 42
      }
    }
  }

  "Quest 11" - {
    "Part 1" - {
      import Quest11A.{Graph, PortalRing, calcPortalResonance}

      "should calculate the difference between the circular and shortcut distances" in {
        val ring: PortalRing = PortalRing(10, List((5, 10), (2, 7)))
        val graph = Graph(ring)
        calcPortalResonance(graph) shouldEqual 60
      }
    }
  }
}
