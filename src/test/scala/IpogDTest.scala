import IpogTypes.{Dimension, ParamComb, Parameter, ValuesComb}
import IpogD._
import munit.FunSuite
class IpogDTest extends FunSuite {

  implicit val tupleToParameter: ((String, Dimension)) => Parameter = (tuple: (String, Dimension)) => Parameter(tuple._1, tuple._2)
  implicit val intToOptionInt: Int => Option[Int] = (n: Int) => Some(n)

  val BOOLEAN_DIMENSION: Dimension = 2

  val parameters: Vector[Parameter] = Vector(("P1", 2), ("P2", 2), ("P3", 2), ("P4", 2), ("P5", 2), ("P6", 2))
  val oddParameters: Vector[Parameter] = Vector(("P1", 2), ("P2", 2), ("P3", 2), ("P4", 2), ("P5", 2), ("P6", 2), ("P7", 2))
  val eightParameters: Vector[Parameter] = Vector(("P1", 3), ("P2", 3), ("P3", 4), ("P4", 3), ("P5", 2), ("P6", 2), ("P7", 2),("P8", 2))

  val TS1: Vector[ValuesComb] = Vector(
    Vector(0, 0, 0),
    Vector(0, 0, 1),
    Vector(0, 1, 0),
    Vector(0, 1, 1),
    Vector(1, 0, 0),
    Vector(1, 0, 1),
    Vector(1, 1, 0),
    Vector(1, 1, 1)
  )

  val TS2: Vector[ValuesComb] = Vector(
    Vector(0, 0, 0),
    Vector(0, 1, 1),
    Vector(1, 0, 1),
    Vector(1, 1, 0)
  )

  val TS: Vector[ValuesComb] = Vector(
    Vector(0, 0, 0, 0, 0, 0),
    Vector(0, 0, 0, 0, 1, 1),
    Vector(0, 0, 1, 1, 0, 0),
    Vector(0, 0, 1, 1, 1, 1),
    Vector(1, 1, 0, 0, 0, 0),
    Vector(1, 1, 0, 0, 1, 1),
    Vector(1, 1, 1, 1, 0, 0),
    Vector(1, 1, 1, 1, 1, 1),
    Vector(0, 1, 0, 1, 0, 1),
    Vector(0, 1, 1, 0, 1, 0),
    Vector(1, 0, 0, 1, 1, 0),
    Vector(1, 0, 1, 0, 0, 1)
  )
  val oddTS1: Vector[ValuesComb] = Vector(
    Vector(0, 0, 0),
    Vector(0, 0, 1),
    Vector(0, 1, 0),
    Vector(0, 1, 1),
    Vector(1, 0, 0),
    Vector(1, 0, 1),
    Vector(1, 1, 0),
    Vector(1, 1, 1)
  )

  val oddTS2: Vector[ValuesComb] = Vector(
    Vector(0, 0, 0),
    Vector(0, 1, 1),
    Vector(1, 0, 1),
    Vector(1, 1, 0)
  )

  val oddTS: Vector[ValuesComb] = Vector(
    Vector(0, 0, 0, 0, 0, 0),
    Vector(0, 0, 0, 0, 1, 1),
    Vector(0, 0, 1, 1, 0, 0),
    Vector(0, 0, 1, 1, 1, 1),
    Vector(1, 1, 0, 0, 0, 0),
    Vector(1, 1, 0, 0, 1, 1),
    Vector(1, 1, 1, 1, 0, 0),
    Vector(1, 1, 1, 1, 1, 1),
    Vector(0, 1, 0, 1, 0, 1),
    Vector(0, 1, 1, 0, 1, 0),
    Vector(1, 0, 0, 1, 1, 0),
    Vector(1, 0, 1, 0, 0, 1)
  )

  val overDimensionsTestSet: Vector[ValuesComb] = Vector(
    Vector(0, 0, 0, 0, 0, 0),
    Vector(0, 0, 0, 0, 1, 1),
    Vector(0, 0, 1, 1, 0, 0),
    Vector(0, 0, 1, 1, 1, 1),
    Vector(1, 2, 0, 0, 0, 0),
    Vector(1, 2, 0, 0, 1, 1),
    Vector(1, 1, 5, 1, 0, 0),
    Vector(1, 1, 1, 8, 1, 1),
    Vector(2, 1, 0, 1, 0, 2),
    Vector(5, 1, 1, 0, 1, 7),
    Vector(1, 0, 0, 1, 1, 0),
    Vector(1, 0, 1, 0, 0, 1)
  )

  val checkedTestSet: Vector[ValuesComb] = Vector(
    Vector(0, 0, 0, 0, 0, 0),
    Vector(0, 0, 0, 0, 1, 1),
    Vector(0, 0, 1, 1, 0, 0),
    Vector(0, 0, 1, 1, 1, 1),
    Vector(1, None, 0, 0, 0, 0),
    Vector(1, None, 0, 0, 1, 1),
    Vector(1, 1, None, 1, 0, 0),
    Vector(1, 1, 1, None, 1, 1),
    Vector(None, 1, 0, 1, 0, None),
    Vector(None, 1, 1, 0, 1, None),
    Vector(1, 0, 0, 1, 1, 0),
    Vector(1, 0, 1, 0, 0, 1)
  )

  test("isUniform should check if all differences between active twin pairs are uniform"){
    val paramCombs: Vector[ParamComb] = Vector(
      Vector(1, 1, 0, 1, 1, 1, 0, 0, 1),
      Vector(1, 1, 1, 1, 1, 0, 1, 0, 0),
      Vector(1, 1, 1, 1, 1, 1, 0, 0)
    )
    val uniformValuesComb: Vector[ValuesComb] = Vector(
      Vector(2, 2, 2, 2, 2, 2),
      Vector(2, 3, 1, 0, 1, 2),
      Vector(2, 4, 2, 0, 2, 0)
    )
    val notUniformValuesComb: Vector[ValuesComb] = Vector(
      Vector(2, 1, 2, 2, 2, 2),
      Vector(2, 4, 0, 1, 1, 2),
      Vector(2, 2, 2, 0, 3, 1)
    )
    val checkUniforms = for {
      paramComb <- paramCombs
      valueComb <- uniformValuesComb
    } yield isUniform(paramComb, valueComb)

    val checkNotUniforms = for {
      paramComb <- paramCombs
      valueComb <- notUniformValuesComb
    } yield isUniform(paramComb, valueComb)

    assert(checkUniforms.forall(_ == true))
    assert(checkNotUniforms.forall(_ == false))
  }

  test("checkDimensions should replace over dimension values with wildCards in a test set") {
    assertEquals(checkDimensions(overDimensionsTestSet, parameters), checkedTestSet)
  }

  test("doublingConstruct should properly double the number of parameters in a t-way test set"){
    assertEquals(doublingConstruct(TS1, TS2, BOOLEAN_DIMENSION), TS)
  }

  test("ipog-D algorithm should return a tuple with parameters and a proper test set for t = 3") {
    val t = 3
    assertEquals(ipogD(parameters, t), (parameters, TS))
  }

  test("ipog-D algorithm should return a tuple with parameters and a proper test set for t > 3") {
    val t = 5
    val (_, ipog2) = ipogD(eightParameters, t)
    assertEquals(ipog2.length, 395)
  }

}
