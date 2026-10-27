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
}
