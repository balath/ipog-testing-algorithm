import Ipog.Parameter
import Main._



class MainTest extends munit.FunSuite {


  test("parseCsvTuples should format csv data for ipog implementation input"){
    val headerRow = Array("P1","P2","P3","P4","P5","P6","t")
    val valuesRow = Array(1,2,1,3,2,2,3)
    val tuples = headerRow zip valuesRow

    val expectedParameters = List(
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



}
