import Ipog._
import munit._

class IpogTest extends FunSuite {

  implicit val tupleToParameter: ((String, Dimension)) => Parameter = (tuple: (String, Dimension)) => Parameter(tuple._1, tuple._2)
  implicit val intToOptionInt: Int => Option[Int] = (n: Int) => Some(n)

  /* Test values for combineParameters */
  val M1: Int = -1
  val M2 = 0
  val M3 = 5
  val M4 = 3
  val N1: Int = -1
  val N2 = 0
  val N3 = 5
  val N4 = 3

  /* Test values for combineValues */
  val PP0: Vector[Parameter] = Vector.empty
  val PP1: Vector[Parameter] = Vector(("P1", 2), ("P2", 3), ("P3", 2), ("P4", 3), ("P5", 2))
  val C0: ParamComb = Vector.empty
  val C1: ParamComb = Vector(0, 0, 0, 0, 0)
  val C2: ParamComb = Vector(1, 1, 1, 1, 1)
  val C3: ParamComb = Vector(1, 1, 0, 0, 1)

  /* Test values for maxCoverageValue */
  val P1: Parameter = ("P3", 2)
  val V1: ValuesComb  = Vector(0)
  val V2: ValuesComb  = Vector(0, 0)
  val V3: ValuesComb  = Vector(0, 1)
  val PI0: PiList = Map.empty
  val PI1: PiList = Map(Vector(0, 1, 1) -> Vector.empty, Vector(1, 0, 1) -> Vector.empty)
  val PI2: PiList = Map(
    Vector(0, 1, 1) -> Vector(Vector(0, 0), Vector(0, 1), Vector(1, 0), Vector(1, 1), Vector(2, 0), Vector(2, 1)),
    Vector(1, 0, 1) ->  Vector(Vector(0, 0), Vector(0, 1), Vector(1, 0), Vector(1, 1), Vector(2, 0), Vector(2, 1))
  )

  /* Test values for equivTo and replaceWildcards */
  val V0: ValuesComb = Vector.empty
  val V4: ValuesComb = Vector(None, None)
  val V5: ValuesComb = Vector(2, 2)
  val V6: ValuesComb = Vector(0, None)
  val V7: ValuesComb = Vector(None, 2)
  val V8: ValuesComb = Vector(2, None, 2)

  /* Test values for updatePi */
  val PI3: PiList =  Map(
    Vector(0, 1, 1) -> Vector(Vector(0, 0)),
    Vector(1, 0, 1) -> Vector(Vector(0, 0))
  )

  test("ipog algorithm should return a tuple with parameter dimensions a proper test set") {
    val inputT = 2
    val expectedParameters: Vector[Parameter] = Vector(("P2", 3), ("P4", 3), ("P1", 2), ("P3", 2), ("P5", 2))
    val expectedTestSet: Vector[ValuesComb] = Vector(
      Vector(0, 0, 0, 0, 0),
      Vector(0, 1, 1, 1, 1),
      Vector(0, 2, 0, 1, 0),
      Vector(1, 0, 1, 0, 1),
      Vector(1, 1, 0, 0, 0),
      Vector(1, 2, 1, 0, 0),
      Vector(2, 0, 0, 1, 1),
      Vector(2, 1, 1, 0, 0),
      Vector(2, 2, 0, 0, 1),
      Vector(1, None, None, 1, 0)
    )
    val output = ipog(PP1, inputT)

    assertEquals(output, (expectedParameters, expectedTestSet))
  }

  test("combineParameters should combine properly m parameters taken n at a time") {
    val validClasses = Vector((M2, N3), (M3, N2), (M3, N3), (M3, N4))
    val invalidClasses = Vector((M4, N3), (M1, N4), (M4, N1))
    val expectedCombinationsList = Vector(
      Vector.empty, //m=0
      Vector(Vector(0, 0, 0, 0, 0)), //n=0
      Vector(Vector(1, 1, 1, 1, 1)), //m=n
      Vector(
        Vector(0, 0, 1, 1, 1),
        Vector(0, 1, 0, 1, 1),
        Vector(0, 1, 1, 0, 1),
        Vector(0, 1, 1, 1, 0),
        Vector(1, 0, 0, 1, 1),
        Vector(1, 0, 1, 0, 1),
        Vector(1, 0, 1, 1, 0),
        Vector(1, 1, 0, 0, 1),
        Vector(1, 1, 0, 1, 0),
        Vector(1, 1, 1, 0, 0)
      ) // m > n
    )
    val validClassesWithExpected = validClasses lazyZip expectedCombinationsList

    /* Test for valid classes */
    validClassesWithExpected.map {
      case ((m, n), expected) => assertEquals(combineParameters(m, n), expected)
    }
    /* Test for invalid classes */
    invalidClasses.map {
      case (m, n) => interceptMessage[IllegalArgumentException]("Parameters less than 0") {
        combineParameters(m, n)
      }
    }
  }

  test("combineValues should return all values combinations of given combination parameters") {

    val expectedForC2: Vector[ValuesComb] = Vector(
      Vector(0, 0, 0, 0, 0),
      Vector(0, 0, 0, 0, 1),
      Vector(0, 0, 0, 1, 0),
      Vector(0, 0, 0, 1, 1),
      Vector(0, 0, 0, 2, 0),
      Vector(0, 0, 0, 2, 1),
      Vector(0, 0, 1, 0, 0),
      Vector(0, 0, 1, 0, 1),
      Vector(0, 0, 1, 1, 0),
      Vector(0, 0, 1, 1, 1),
      Vector(0, 0, 1, 2, 0),
      Vector(0, 0, 1, 2, 1),
      Vector(0, 1, 0, 0, 0),
      Vector(0, 1, 0, 0, 1),
      Vector(0, 1, 0, 1, 0),
      Vector(0, 1, 0, 1, 1),
      Vector(0, 1, 0, 2, 0),
      Vector(0, 1, 0, 2, 1),
      Vector(0, 1, 1, 0, 0),
      Vector(0, 1, 1, 0, 1),
      Vector(0, 1, 1, 1, 0),
      Vector(0, 1, 1, 1, 1),
      Vector(0, 1, 1, 2, 0),
      Vector(0, 1, 1, 2, 1),
      Vector(0, 2, 0, 0, 0),
      Vector(0, 2, 0, 0, 1),
      Vector(0, 2, 0, 1, 0),
      Vector(0, 2, 0, 1, 1),
      Vector(0, 2, 0, 2, 0),
      Vector(0, 2, 0, 2, 1),
      Vector(0, 2, 1, 0, 0),
      Vector(0, 2, 1, 0, 1),
      Vector(0, 2, 1, 1, 0),
      Vector(0, 2, 1, 1, 1),
      Vector(0, 2, 1, 2, 0),
      Vector(0, 2, 1, 2, 1),
      Vector(1, 0, 0, 0, 0),
      Vector(1, 0, 0, 0, 1),
      Vector(1, 0, 0, 1, 0),
      Vector(1, 0, 0, 1, 1),
      Vector(1, 0, 0, 2, 0),
      Vector(1, 0, 0, 2, 1),
      Vector(1, 0, 1, 0, 0),
      Vector(1, 0, 1, 0, 1),
      Vector(1, 0, 1, 1, 0),
      Vector(1, 0, 1, 1, 1),
      Vector(1, 0, 1, 2, 0),
      Vector(1, 0, 1, 2, 1),
      Vector(1, 1, 0, 0, 0),
      Vector(1, 1, 0, 0, 1),
      Vector(1, 1, 0, 1, 0),
      Vector(1, 1, 0, 1, 1),
      Vector(1, 1, 0, 2, 0),
      Vector(1, 1, 0, 2, 1),
      Vector(1, 1, 1, 0, 0),
      Vector(1, 1, 1, 0, 1),
      Vector(1, 1, 1, 1, 0),
      Vector(1, 1, 1, 1, 1),
      Vector(1, 1, 1, 2, 0),
      Vector(1, 1, 1, 2, 1),
      Vector(1, 2, 0, 0, 0),
      Vector(1, 2, 0, 0, 1),
      Vector(1, 2, 0, 1, 0),
      Vector(1, 2, 0, 1, 1),
      Vector(1, 2, 0, 2, 0),
      Vector(1, 2, 0, 2, 1),
      Vector(1, 2, 1, 0, 0),
      Vector(1, 2, 1, 0, 1),
      Vector(1, 2, 1, 1, 0),
      Vector(1, 2, 1, 1, 1),
      Vector(1, 2, 1, 2, 0),
      Vector(1, 2, 1, 2, 1)
    )
    val expectedForC3: Vector[ValuesComb] = Vector(
      Vector(0, 0, 0),
      Vector(0, 0, 1),
      Vector(0, 1, 0),
      Vector(0, 1, 1),
      Vector(0, 2, 0),
      Vector(0, 2, 1),
      Vector(1, 0, 0),
      Vector(1, 0, 1),
      Vector(1, 1, 0),
      Vector(1, 1, 1),
      Vector(1, 2, 0),
      Vector(1, 2, 1)
    )

    assertEquals(combineValues(PP1, C0), Vector(Vector.empty))
    assertEquals(combineValues(PP0, C1), Vector(Vector.empty))
    assertEquals(combineValues(PP1, C2), expectedForC2)
    assertEquals(combineValues(PP1, C3), expectedForC3)
    assertEquals(combineValues(PP1, C1), Vector(Vector.empty))
  }

  test("maxCoverageValue should find the value that makes test cases set cover the most combinations") {
    val piListForV3PI2: PiList = Map(
      Vector(0, 1, 1) -> Vector(Vector(1, 0)),
      Vector(1, 0, 1) -> Vector(Vector(0, 0))
    )

    assertEquals(maxCoverageValue(P1, V1, PI1), (0, PI1)) //0 by default and a PiList with empty values
    assertEquals(maxCoverageValue(P1, V2, PI2), (0, PI3)) //0 with 2 covered combinations
    assertEquals(maxCoverageValue(P1, V2, PI1), (0, PI1)) //0 by default and a PiList with empty values
    assertEquals(maxCoverageValue(P1, V3, PI1), (0, PI1)) //0 by default and a PiList with empty values
    assertEquals(maxCoverageValue(P1, V1, PI2), (0, PI1)) //0 by default and a PiList with empty values
    assertEquals(maxCoverageValue(P1, V3, PI2), (0, piListForV3PI2)) //0 with 2 covered combinations

    intercept[UnsupportedOperationException] {
      maxCoverageValue(P1, V3, PI0)
    }
  }

  test("equivTo should be true when compared combinations are equivalents") {
    assert(!equivTo(V0,V4)) //not equivalent
    assert(equivTo(V4,V2))  //equivalent
    assert(!equivTo(V2,V5)) //not equivalent
    assert(!equivTo(V5,V6)) //not equivalent
    assert(equivTo(V6,V7))  //equivalent
    assert(!equivTo(V7,V8)) //not equivalent
    assert(!equivTo(V8,V0)) //not equivalent
  }

  test("replaceWildCards should return a combination with minimal number of  wildcards, given two equivalent combinations") {
    val expectedForV7V6: ValuesComb = Vector(0, 2)

    assertEquals(replaceWildcards(V4, V2), V2)
    assertEquals(replaceWildcards(V2, V6), V2)
    assertEquals(replaceWildcards(V5, V7), V5)
    assertEquals(replaceWildcards(V6, V4), V6)
    assertEquals(replaceWildcards(V7, V6), expectedForV7V6)
  }

  test("updatePi should delete covered combinations from the pi list") {

    val emptyPi: PiList = Map.empty
    val expectedForPI2PI3: PiList = Map(
      Vector(0, 1, 1) -> Vector(Vector(0, 1), Vector(1, 0), Vector(1, 1), Vector(2, 0), Vector(2, 1)),
      Vector(1, 0, 1) -> Vector(Vector(0, 1), Vector(1, 0), Vector(1, 1), Vector(2, 0), Vector(2, 1))
    )

    assertEquals(updatePi(PI0, PI1), emptyPi)
    assertEquals(updatePi(PI1, PI2), PI1)
    assertEquals(updatePi(PI2, PI3), expectedForPI2PI3)
    assertEquals(updatePi(PI3, PI0), PI3)
  }

  test("getPiRemains should flat the pi List and assign wildcards to non active parameters") {
    val expectedForPI0: Vector[ValuesComb]  = Vector.empty
    val expectedForPI1: Vector[ValuesComb]  = Vector.empty
    val expectedForPI2: Vector[ValuesComb]  = Vector(
      Vector(None, 0, 0),
      Vector(None, 0, 1),
      Vector(None, 1, 0),
      Vector(None, 1, 1),
      Vector(None, 2, 0),
      Vector(None, 2, 1),
      Vector(0, None, 0),
      Vector(0, None, 1),
      Vector(1, None, 0),
      Vector(1, None, 1),
      Vector(2, None, 0),
      Vector(2, None, 1)
    )
    val expectedForPI3: Vector[ValuesComb] = Vector(Vector(None, 0, 0),Vector(0, None, 0))

    assertEquals(getPiRemains(PI0), expectedForPI0)
    assertEquals(getPiRemains(PI1), expectedForPI1)
    assertEquals(getPiRemains(PI2), expectedForPI2)
    assertEquals(getPiRemains(PI3), expectedForPI3)
  }

  test("verticalExtend should reuse equivalents combinations with wildcards") {
    val piList: PiList = Map(
      Vector(0, 0, 0, 1, 1) -> Vector.empty,
      Vector(0, 0, 1, 0, 1) -> Vector(Vector(0, 1), Vector(1, 0)),
      Vector(0, 1, 0, 0, 1) -> Vector.empty,
      Vector(1, 0, 0, 0, 1) -> Vector.empty,
    )
    val testSet: Vector[ValuesComb] = Vector(
      Vector(0, 0, 0, 0, 0),
      Vector(0, 1, 1, 1, 1),
      Vector(1, 0, 1, 0, 1),
      Vector(1, 1, 0, 1, 0),
      Vector(None, 0, None, 1, 0),
      Vector(None, 1, None, 0, 0)
    )
    val expected: Vector[ValuesComb]= Vector(
      Vector(0, 0, 0, 0, 0),
      Vector(0, 1, 1, 1, 1),
      Vector(1, 0, 1, 0, 1),
      Vector(1, 1, 0, 1, 0),
      Vector(None, 0, 1, 1, 0),
      Vector(None, 1, None, 0, 0),
      Vector(None, None, 0, None, 1)
    )
    val piRemains = getPiRemains(piList)
    val obtained = verticalExtension(testSet, piRemains)

    assertEquals(obtained, expected)
  }

}


