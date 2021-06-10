import Ipog._
import Utils.nCr

class IpogTest extends munit.FunSuite {

  val inputParameters = Vector(("P1",2),("P2",3),("P3",2),("P4",3),("P5",2)).map(p => Parameter(p._1, p._2))
  val inputPi = Map(
    (Vector(0, 1, 1),
      Vector(Vector(0, 0), Vector(0, 1), Vector(1, 0), Vector(1, 1), Vector(2, 0), Vector(2, 1)).map(_.map(Some(_)))),
    (Vector(1, 0, 1),
      Vector(Vector(0, 0), Vector(0, 1), Vector(1, 0), Vector(1, 1), Vector(2, 0), Vector(2, 1)).map(_.map(Some(_))))
  )
  val coveredCombinations = Map(
    (Vector(0, 1, 1),
      Vector(Vector(Some(0), Some(0)))),
    (Vector(1, 0, 1),
      Vector(Vector(Some(0), Some(0))))
  )

  test("ipog algorithm should return a tuple with parameter dimensions a proper test set") {
    val inputT = 2
    val expectedParameters = Vector(("P2",3),("P4",3),("P1",2),("P3",2),("P5",2)).map(p => Parameter(p._1, p._2))
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
    val output = ipog(inputParameters,inputT)
    assertEquals(output,(expectedParameters,expectedTestSet))
  }

  test("combineParameters should combine properly m parameters taken n at a time") {
    val mList = Vector(5,5,5,0)
    val nList = Vector(3,0,5,3)
    val expectedCombinationsList = Vector(
      Vector(                     //Resultado del ejemplo del enunciado
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
      ),
      Vector(Vector(0,0,0,0,0)),  //Resultado para n=0
      Vector(Vector(1,1,1,1,1)),  //Resultado para m=n
      Vector.empty                //Resultado para m=0
    )

    val testCases = mList lazyZip nList lazyZip expectedCombinationsList
    testCases.map {
      case (m, n, expected) => assertEquals(combineParameters(m, n), expected)
    }
  }

  test("combineParameters should return m nCr n combinations") {
    val inputM = 7
    val inputN = 4
    val expectedCombinationsNCR = nCr(inputM,inputN)

    assertEquals(Option(combineParameters(inputM, inputN).length.toLong),expectedCombinationsNCR)
  }

  test("combineValues should return all values combinations of given combination parameters") {
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
    ).map(_.map(Some(_)))
    val inputCombOneParameter = Vector(0,1,0,0,0)
    val expectedForOneParameter = Vector(0,1,2).map(n => Vector(Some(n)))
    val inputCombZeroParameter = Vector(0,0,0,0,0)
    val expectedForZeroParameter = Vector(Vector.empty)

    assertEquals(combineValues(inputParameters,inputCombination),expectedCombinations)
    assertEquals(combineValues(inputParameters,inputCombOneParameter),expectedForOneParameter)
    assertEquals(combineValues(inputParameters,inputCombZeroParameter),expectedForZeroParameter)
  }

  test("maxCoverageValue should find the value that makes test cases set cover the most combinations") {
    val inputParameter = Parameter("P1",2)
    val row = Vector(0,0)
    val expected = (0, coveredCombinations)

    assertEquals(maxCoverageValue(inputParameter,row.map(Some(_)),inputPi),expected)
  }

  test("updatePi should delete covered combinations from the pi list") {

    val emptyPi: PiList = Map.empty
    val emptyCoveredCombinations: PiList = Map.empty
    val expected = Map(
    (Vector(0, 1, 1),
    Vector(Vector(0, 1), Vector(1, 0), Vector(1, 1), Vector(2, 0), Vector(2, 1)).map(_.map(Some(_)))),
    (Vector(1, 0, 1),
    Vector(Vector(0, 1), Vector(1, 0), Vector(1, 1), Vector(2, 0), Vector(2, 1)).map(_.map(Some(_))))
    )

    assertEquals(updatePi(inputPi,coveredCombinations), expected)
    assertEquals(updatePi(inputPi,emptyCoveredCombinations), inputPi)
    assertEquals(updatePi(emptyPi,coveredCombinations), emptyPi)
  }

  test("verticalExtend should reuse equivalents combinations with wildcards") {
    val piList = Map(
      (Vector(0, 0, 0, 1, 1),
        Vector.empty),
      (Vector(0, 0, 1, 0, 1),
        Vector(Vector(0, 1), Vector(1, 0)).map(_.map(Some(_)))),
      (Vector(0, 1, 0, 0, 1),
          Vector.empty),
      (Vector(1, 0, 0, 0, 1),
        Vector.empty),
    )
    val testSet = Vector(
      Vector(0,0,0,0,0),
      Vector(0,1,1,1,1),
      Vector(1,0,1,0,1),
      Vector(1,1,0,1,0),
      Vector(-1,0,-1,1,0),
      Vector(-1,1,-1,0,0)
    ).map(_.map {
      case -1 => None
      case n => Some(n)
    })

    val expected = Vector(
      Vector(0,0,0,0,0),
      Vector(0,1,1,1,1),
      Vector(1,0,1,0,1),
      Vector(1,1,0,1,0),
      Vector(-1,0,1,1,0),
      Vector(-1,1,-1,0,0),
      Vector(-1,-1,0,-1,1)
    ).map(_.map {
      case -1 => None
      case n => Some(n)
    })

    val piLeftovers = getPiRemains(piList)
    val obtained = verticalExtension(testSet, piLeftovers)

    assertEquals(obtained, expected)
  }

}


