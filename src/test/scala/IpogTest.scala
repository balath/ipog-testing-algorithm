import Ipog._
import Utils.nCr

class IpogTest extends munit.FunSuite {

  test("ipog algorithm should return a tuple with parameter dimensions a proper test set".ignore) {
    val inputParameters = Vector(("P1",2),("P2",3),("P3",2),("P4",3),("P5",2)).map(param => Parameter(param._1, param._2))
    val inputT = 2
    val expectedParameters = Vector(("P2",3),("P4",3),("P1",2),("P3",2),("P5",2)).map(param => Parameter(param._1, param._2))
    val expectedTestSet = Vector(
      Vector(0,0,0,0,0),
      Vector(0,1,1,1,1),
      Vector(0,2,0,1,0),
      Vector(1,0,1,0,1),
      Vector(1,1,0,0,0),
      Vector(1,2,1,0,0),
      Vector(2,0,0,1,1),
      Vector(2,1,1,0,0),
      Vector(2,2,0,0,1),
      Vector(1,-1,-1,1,0)
    ).map(_.map{
      case -1 => None
      case n => Some(n)
    })
    assertEquals(ipog(inputParameters,inputT),(expectedParameters,expectedTestSet))
  }

  test("combineParameters should combine properly m parameters taken n at a time") {
    val inputM = 5
    val inputN = 3
    val expectedCombinations = Vector(
      Vector(0,0,1,1,1),
      Vector(0,1,0,1,1),
      Vector(0,1,1,0,1),
      Vector(0,1,1,1,0),
      Vector(1,0,0,1,1),
      Vector(1,0,1,0,1),
      Vector(1,0,1,1,0),
      Vector(1,1,0,0,1),
      Vector(1,1,0,1,0),
      Vector(1,1,1,0,0)
    )
    assertEquals(combineParameters(inputM,inputN),expectedCombinations)
  }

  test("combineParameters should return a 0 filled vector when n = 0") {
    val inputM = 5
    val inputN = 0
    val expectedCombinations = Vector(
      Vector(0,0,0,0,0)
    )
    assertEquals(combineParameters(inputM,inputN),expectedCombinations)
  }

  test("combineParameters should return a 1 filled vector when m = n") {
    val sameInput = 5
    val expectedCombinations = Vector(
      Vector(1,1,1,1,1)
    )
    assertEquals(combineParameters(sameInput,sameInput),expectedCombinations)
  }

  test("combineParameters should return an empty vector when m = 0") {
    val inputM = 0
    val inputN = 0
    val expectedCombinations = Vector.empty
    assertEquals(combineParameters(inputM,inputN),expectedCombinations)
  }

  test("combineParameters should return m nCr n combinations") {
    val inputM = 7
    val inputN = 4
    val expectedCombinations = nCr(inputM,inputN)
    assertEquals(combineParameters(inputM,inputN).length.toLong,expectedCombinations)
}
  test("combineValues should return all values combinations of given combination parameters") {
    val inputParameters = Vector(("P1",2),("P2",3),("P3",2),("P4",3),("P5",2)).map(param => Parameter(param._1, param._2))
    val inputCombination = Vector(1,1,0,0,1)
    val expectedCombinations = Vector(
      Vector(0,0,0),
      Vector(0,0,1),
      Vector(0,1,0),
      Vector(0,1,1),
      Vector(0,2,0),
      Vector(0,2,1),
      Vector(1,0,0),
      Vector(1,0,1),
      Vector(1,1,0),
      Vector(1,1,1),
      Vector(1,2,0),
      Vector(1,2,1)
    )
    assertEquals(combineValues(inputParameters,inputCombination),expectedCombinations)
  }

  test("combineValues should return combinations given only one active parameter") {
    val inputParameters = Vector(("P1",2),("P2",3),("P3",2),("P4",3),("P5",2)).map(param => Parameter(param._1, param._2))
    val inputCombination = Vector(0,1,0,0,0)
    val expectedCombinations = Vector(
      Vector(0),
      Vector(1),
      Vector(2)
    )
    assertEquals(combineValues(inputParameters,inputCombination),expectedCombinations)
  }

  test("combineValues should return empty combinations given only none active parameter") {
    val inputParameters = Vector(("P1",2),("P2",3),("P3",2),("P4",3),("P5",2)).map(param => Parameter(param._1, param._2))
    val inputCombination = Vector(0,0,0,0,0)
    val expectedCombinations = Vector(Vector.empty)
    assertEquals(combineValues(inputParameters,inputCombination),expectedCombinations)
  }



}


