import Ipog._
import Combiners._
import IpogTypes._
import com.typesafe.scalalogging.Logger

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

object IpogD {

  val log: Logger = Logger("IPOG-D")


  def doublingConstruct(testSet1: Vector[ValuesComb], testSet2: Vector[ValuesComb], dimension: Dimension): Vector[ValuesComb] = {
    val firstStep = testSet1.map(_.flatMap(value => List(value, value)))
    val secondStep = for {
      i <- 1 until dimension
      combination <- testSet2
      expandedCombination = combination.flatMap {
        case Some(value) => List(Some(value), Some((value + i) % dimension))
        case None => List(None, None)
      }
    } yield expandedCombination
    firstStep ++ secondStep
  }

  /**
   * Revisa el juego de test en busca de valores que exceden de la dimensión de cada parámetro, en su columna
   * correspondiente, sustituyéndolo por un valor comodín. Al realizarse esta comprobación emparejando en una tupla
   * el valor y el parámetro, en caso de que los parámetros sean impares, se desecha la última columna gemela.
   */
  def checkDimensions(testSet: Vector[ValuesComb], parameters: Vector[Parameter]): Vector[ValuesComb] = for {
    valuesCombination <- testSet
    newValueComb = (valuesCombination lazyZip parameters).map {
      case (Some(value), Parameter(_, dimension)) if value >= dimension => None
      case (Some(value), _) => Some(value)
      case _ => None
    }
  } yield newValueComb

  def maxCoverageValue(currentParameter: Parameter, currentParamIndex: Int, currentRow: ValuesComb, pi: PiList): (Int, PiList) = {
    val valueAndCoveredCombinations = for {
      value <- 0 until currentParameter.dimension
      (parameters, values) <- pi
      newRow = currentRow.updated(currentParamIndex, Some(value)) //En vez de añadir el valor al final, se sustituye por el None
      matchedParametersValues = (parameters zip newRow).filter(_._1 == 1).map(_._2)
      coveredValues = values.filter(equivTo(_, matchedParametersValues))
      matches = coveredValues.length
    } yield (value, matches, parameters -> coveredValues)

    val (maxValue, (_, coveredCombinations)): (Int, (Any, PiList)) = valueAndCoveredCombinations
      .groupMapReduce(_._1)(tuple => (tuple._2, Map(tuple._3)))((comb1, comb2) => (comb1._1 + comb2._1, comb1._2 ++ comb2._2))
      .maxByOption(_._2._1).getOrElse(0, (Vector.empty, Map.empty))

    (maxValue, coveredCombinations)
  }

  def isNotUniform(paramComb: ParamComb, valuesComb: ValuesComb): Boolean = {
    def getDifferences(paramComb: ParamComb, valuesComb: ValuesComb): Vector[Int] = (paramComb, valuesComb) match {
      case (p1 +: p2 +: ps, Some(v1) +: Some(v2) +: vs) if p1 == 1 && p2 == 1 => (v1 - v2).abs +: getDifferences(ps, vs)
      case (p1 +: p2 +: ps, _ +: vs) if p1 == 1 || p2 == 1 => getDifferences(ps, vs)
      case (_ +: _ +: ps, vs) => getDifferences(ps, vs)
      case _ => Vector.empty
    }

    val differences = getDifferences(paramComb, valuesComb)
    differences == differences.distinct
  }

  def ipogD(parameters: Vector[Parameter], t: Int): (Vector[Parameter], Vector[ValuesComb]) = {
    val sortedParameters = parameters.sortBy(_.dimension)(Ordering.Int.reverse)
    val paramsLength = sortedParameters.length
    val g1 = sortedParameters.zipWithIndex.filter(_._2 % 2 == 0).map(_._1)
    val (_, ts1) = ipog(g1, t)
    val (_, ts2) = ipog(g1, t - 1)
    val ts = checkDimensions(doublingConstruct(ts1, ts2, g1.head.dimension), sortedParameters)
    val originalTestSize = ts.length

    @tailrec
    def extend(ts: Vector[ValuesComb], newParamIndex: Int, currentTestSize: Dimension): Vector[ValuesComb] = {
      val twinPairsCovered = ((newParamIndex + 1) / 2) - 1

      def horizontalExtension(testSet: Vector[ValuesComb], piList: PiList, iter: Int): Vector[ValuesComb] = testSet match {
        case head +: tail if head(newParamIndex).isEmpty && (iter < originalTestSize) =>
          val (newValue, coveredValues) = maxCoverageValue(sortedParameters(newParamIndex), newParamIndex, head, piList)
          val updatedPiList = updatePi(piList, coveredValues)
          val newRow = head.updated(newParamIndex, Some(newValue))
//          log.info(s"Updated parameter ${newParamIndex + 1} in row $newRow")
          newRow +: horizontalExtension(tail, updatedPiList, iter + 1)
        case head +: tail if head(newParamIndex).isEmpty =>
          val newRow = head.updated(newParamIndex, Some(0))
          newRow +: horizontalExtension(tail, piList, iter + 1)
        case head +: tail => head +: horizontalExtension(tail, piList, iter + 1)
        case _ => getPiRemains(piList)
      } //End of horizontalExtension function

      @tailrec
      def updateSinglesParams(paramComb: ParamComb, nonActiveParams: Vector[(Int, Int)], acc: Vector[(Int, Int)]): Vector[(Int, Int)] =
      (paramComb, nonActiveParams) match {
          case (x +: xs, y +: ys) if (y._2 % 2 == 0) || (y._2 < newParamIndex) => updateSinglesParams(xs, ys, acc :+ (x, y._2))
          case (_, y +: ys) => updateSinglesParams(paramComb, ys, acc :+ y)
          case _ => acc
        } //End of updateSinglesParams function

      def combinePi(twinPairsCombsRange: Range, currentTwinPair: Vector[Int]): PiList = (for {
        n <- twinPairsCombsRange
        twinPairsComb <- combineParameters(twinPairsCovered, n)

        paramsAfterCurrent = paramsLength - (newParamIndex + 1)
        twinPairsCompleted = twinPairsComb.flatMap(n => Vector(n, n)) ++ currentTwinPair ++ Vector.fill(paramsAfterCurrent)(0)
        (activeParamsWithIndex, nonActiveParamsWithIndex) = twinPairsCompleted.zipWithIndex.partition(_._1 == 1)
        singlesParamsWithIndex = nonActiveParamsWithIndex.count { case (_, index) => (index % 2 == 0) || (index < newParamIndex) }
        singlesCombinations = combineParameters(singlesParamsWithIndex, t - activeParamsWithIndex.length)
        singlesCombsWithIndex = singlesCombinations.map(updateSinglesParams(_, nonActiveParamsWithIndex, Vector.empty))

        singlesComb <- singlesCombsWithIndex

        sortedParametersComb = (activeParamsWithIndex ++ singlesComb).sortBy(_._2).map(_._1)
        nonUniformValuesComb = combineValues(sortedParameters, sortedParametersComb).filter(isNotUniform(sortedParametersComb, _))
      } yield sortedParametersComb -> nonUniformValuesComb).toMap
      //End of combinePi function

      lazy val piOne = combinePi(1 until t / 2, Vector(1, 1))
      lazy val piTwo = combinePi(2 to t / 2, Vector(0, 1))
      lazy val piList = piOne ++ piTwo

      Try(sortedParameters(newParamIndex)) match {
        case Failure(_) => ts
        case Success(_) if newParamIndex % 2 == 0 => extend(ts, newParamIndex + 1, ts.length)
        case Success(_) =>
          val horizontalExtensionResult = horizontalExtension(ts, piList, 0)
          val (horizontalExtendedSet, piRemains) = horizontalExtensionResult.splitAt(currentTestSize)
          val verticalExtendedSet = verticalExtension(horizontalExtendedSet, piRemains)
          extend(verticalExtendedSet, newParamIndex + 1, verticalExtendedSet.length)
      }
    } //End of extend function

    t match {
      case 3 => (sortedParameters, ts)
      case _ => (sortedParameters, extend(ts, 1, originalTestSize))
    }

  } //End of ipog function
}