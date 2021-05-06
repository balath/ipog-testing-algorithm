

class UtilsTest extends munit .FunSuite {

  test("Factorial function returns right results for 0,1,2,5 and 10"){
    val inputCases = List(0,1,2,5,10)
    val expectedResults: List[Long] = List(1,1,2,120,3628800)
    val output: List[Long] = inputCases.map(Utils.factorial(_))
    assertEquals(output,expectedResults)
  }

  test("nCr function returns binomial coefficient (n choose r) of two numbers") {
    val inputCases =                    List((5,0),   (0,5),    (5,5),  (5,3),    (7,5),    (-2,5), (2,5),  (2,-5))
    val expected: List[Option[Long]] =  List(Some(1), Some(0), Some(1), Some(10), Some(21), None,   None,   None)
    val output = inputCases.map(tuple => Utils.nCr(tuple._1, tuple._2))
    assertEquals(output,expected)
  }
}
