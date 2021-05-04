import Ipog.combineParameters
import Utils.nCr
import org.scalacheck.Prop.forAll
import org.scalacheck.Gen
import munit.ScalaCheckSuite

class IpogTestProperties extends ScalaCheckSuite {

  val mnPair = for {
    m <- Gen.choose(1,10)
    n <- Gen.choose(1, m)
  } yield (m,n)

  property(
    "combineParameters should return (m nCr n) combinations, all distinct, and with only n 1's") {
    forAll(mnPair) { case (m,n) =>
      val combinations = combineParameters(m, n)
      val combinationsLength = combinations.length

      assertEquals(Option(combinationsLength.toLong),nCr(m, n))
      assert(combinations.map(_ count (_ == 1)).forall (_ == n))
      assertEquals(combinationsLength, combinations.distinct.length)
    }
  }


}
