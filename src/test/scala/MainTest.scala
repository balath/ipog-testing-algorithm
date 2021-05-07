import Ipog.Parameter
import Main._



class MainTest extends munit.FunSuite {


  test("parseCsvTuples should format csv data for ipog implementation input"){
    val headerRow = Array("P1","P2","P3","P4","P5","P6","t")
    val valuesRow = Array(1,2,1,3,2,2,3)
    val tuples = headerRow zip valuesRow

    val expectedParameters = Vector(
      Parameter("P1",1),
      Parameter("P2",2),
      Parameter("P3",1),
      Parameter("P4",3),
      Parameter("P5",2),
      Parameter("P6",2),
    )
    val expectedT = 3

    assertEquals(parseCsvTuples(tuples),(expectedParameters,expectedT))
  }

  test("testSetToString should return a formatted string for display"){
    val inputParameters = Vector(("P2",3),("P4",3),("P1",2),("P3",2),("P5",2)).map(p => Parameter(p._1, p._2))
    val inputTestSet = Vector(
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
    val expected =
      "=======================\n" +
      "   P2  P4  P1  P3  P5  \n" +
      "=======================\n" +
      "   0   0   0   0   0\n" +
      "   0   1   1   1   1\n" +
      "   0   2   0   1   0\n" +
      "   1   0   1   0   1\n" +
      "   1   1   0   0   0\n" +
      "   1   2   1   0   0\n" +
      "   2   0   0   1   1\n" +
      "   2   1   1   0   0\n" +
      "   2   2   0   0   1\n" +
      "   1   *   *   1   0"

    assertEquals(testSetToString(inputParameters,inputTestSet), expected)
  }

}
