import org.scalatest._
import org.scalatest.prop._
import org.scalacheck.Gen

class QuickselectSpec extends FlatSpec with GeneratorDrivenPropertyChecks with Matchers {
  def testSelect(selector: (Int, Array[Int]) => Option[Int]) {
    it should "return None when passed an empty array" in {
      selector(0, Array.empty) shouldBe None
    }
    it should "return correctly for a small example" in {
      selector(3, Array(7, 10, 4, 3, 20, 15)) shouldBe Some(7)
    }

    val selectorInput =
      for {
        s <- Gen.nonEmptyContainerOf[Set, Int](Gen.choose(0, Int.MaxValue))
        k <- Gen.choose(1, s.size - 1)
      } yield (k, s)

    it should "be the same as taking the (k-1)th element of sort" in {
      forAll(selectorInput) { case (k, xsSet) =>
        whenever (k > 0 && k < xsSet.size) {
          val sorted = xsSet.toSeq.sorted
          val xs: Array[Int] = util.Random.shuffle(sorted).toArray
          selector(k, xs) should be === sorted.lift(k - 1)
        }
      }
    }
  }

  import Quickselect._

  "sortselect" should behave like testSelect(sortselect)
  "quickselect" should behave like testSelect(quickselect)
}
