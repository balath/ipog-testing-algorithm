import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}
import IpogTypes._

object Ipog {

  def combineParameters(m: Int, n: Int): Vector[ParamComb] = {
    @tailrec
    def combine(lastCombination: ParamComb, acc: Vector[ParamComb]): Vector[ParamComb] = {
      (lastCombination zip lastCombination.tail) lastIndexOf ((0, 1)) match {
        case -1 => acc
        case g =>
          lastCombination.last match {
            case 1 =>
              val newCombination = lastCombination.updated(g, 1).updated(g + 1, 0)
              combine(newCombination, acc :+ newCombination)
            case 0 =>
              val r = lastCombination take (g + 1) count (_ == 1)
              val lastOnes = n - r - 1
              val newCombination = lastCombination.lazyZip(0 to m).map {
                case (_, index) if index >= m - lastOnes => 1
                case (_, index) if index > g => 0
                case (_, index) if index == g => 1
                case (elem, _) => elem
              }
              combine(newCombination, acc :+ newCombination)
          }
      }
    }

    (m, n) match {
      case (0, _) => Vector.empty
      case (m, n) if m < 0 || n < 0 || m < n => Vector.empty
      case _ =>
        val initialCombination = Vector.fill(m - n)(0) ++ Vector.fill(n)(1)
        combine(initialCombination, Vector(initialCombination))
    }
  }

  def combineValues(parameters: Vector[Parameter], combination: ParamComb): Vector[ValuesComb] = {
    @tailrec
    def combine(dimensions: Vector[Dimension], acc: Vector[ValuesComb]): Vector[ValuesComb] = dimensions match {
      case dimension +: tail =>
        val newVector = for {
          accElem <- acc
          newElem <- 0 until dimension
        } yield accElem :+ Some(newElem)
        combine(tail, newVector)
      case _ => acc
    }

    val dimensions = (parameters zip combination).filter(_._2 == 1).map { case (Parameter(_, dimension), _) => dimension }
    combine(dimensions, Vector(Vector.empty))
  }

  def maxCoverageValue(currentParameter: Parameter, currentRow: ValuesComb, pi: PiList): (Int, PiList) = {
    val valueAndCoveredCombinations = for {
      value <- 0 until currentParameter.dimension
      (parameters, values) <- pi
      newRow = currentRow :+ Some(value)
      matchedParametersValues = (parameters zip newRow).filter(_._1 == 1).map(_._2)
      coveredValues = values.filter(equivTo(_, matchedParametersValues))
      matches = coveredValues.length
    } yield (value, matches, parameters -> coveredValues)

    val (maxValue, (_, coveredCombinations)) = valueAndCoveredCombinations
      .groupMapReduce(_._1)(tuple => (tuple._2, Map(tuple._3)))((comb1, comb2) => (comb1._1 + comb2._1, comb1._2 ++ comb2._2))
      .maxBy(_._2._1)

    (maxValue, coveredCombinations)
  }

  @scala.annotation.tailrec
  def equivTo(comb1: ValuesComb, comb2: ValuesComb): Boolean = (comb1, comb2) match {
    case (Vector(), Vector()) => true
    case (None +: c1tail, _ +: c2tail) => equivTo(c1tail, c2tail)
    case (_ +: c1tail, None +: c2tail) => equivTo(c1tail, c2tail)
    case (Some(n1) +: c1tail, Some(n2) +: c2tail) => (n1 == n2) && equivTo(c1tail, c2tail)
    case _ => false
  }

  def replaceWildcards(comb1: ValuesComb, comb2: ValuesComb): ValuesComb = (comb1 lazyZip comb2).map {
    case (None, None) => None
    case (None, Some(n)) => Some(n)
    case (Some(n), None) => Some(n)
    case (Some(n), Some(_)) => Some(n)
  }

  def updatePi(piList: PiList, coveredCombinations: PiList): PiList = for {
    (parameters, values) <- piList
    updatedValues = values.filterNot(coveredCombinations.getOrElse(parameters, Vector.empty).contains(_))
  } yield parameters -> updatedValues

  def getPiRemains(piList: PiList): Vector[ValuesComb] = {
    def insertWildcards(parameters: ParamComb, row: ValuesComb): ValuesComb = parameters match {
      case 0 +: tail => None +: insertWildcards(tail, row)
      case _ +: tail => row.head +: insertWildcards(tail, row.tail)
      case _ => Vector.empty
    }

    piList.flatMap { case (parameters, values) => values.map(insertWildcards(parameters, _)) }.toVector
  }

  @tailrec
  def verticalExtension(testSet: Vector[ValuesComb], piRemains: Vector[ValuesComb]): Vector[ValuesComb] =
    piRemains match {
      case head +: tail =>
        testSet.indexWhere(equivTo(_, head)) match {
          case -1 => verticalExtension(testSet :+ head, tail)
          case n => verticalExtension(testSet.updated(n, replaceWildcards(testSet(n), head)), tail)
        }
      case _ => testSet
    }

  def ipog(parameters: Vector[Parameter], t: Int): (Vector[Parameter], Vector[ValuesComb]) = {
    val sortedParameters = parameters.sortBy(_.dimension)(Ordering.Int.reverse)
    val parametersNum = parameters.length
    val combinations = Vector.fill(t)(1) concat Vector.fill(parametersNum - t)(0)
    val testSet = combineValues(sortedParameters, combinations)
    val originalTestSize = testSet.length

    @tailrec
    def extend(testSet: Vector[ValuesComb], newParamIndex: Int, currentTestSize: Int): Vector[ValuesComb] = {
      def horizontalExtension(testSet: Vector[ValuesComb], piList: PiList, iter: Int): Vector[ValuesComb] = testSet match {
        case head +: tail if iter < originalTestSize =>
          val (newValue, coveredValues) = maxCoverageValue(sortedParameters(newParamIndex), head, piList)
          val updatedPiList = updatePi(piList, coveredValues)
          val newRow = head :+ Some(newValue)
          newRow +: horizontalExtension(tail, updatedPiList, iter + 1)
        case head +: tail =>
          val newRow = head :+ Some(0)
          newRow +: horizontalExtension(tail, piList, iter + 1)
        case _ => getPiRemains(piList)
      }

      val piList = (for {
        parametersComb <- combineParameters(newParamIndex + 1, t)
        if parametersComb(newParamIndex) == 1
      } yield parametersComb -> combineValues(sortedParameters, parametersComb)).toMap
      Try(sortedParameters(newParamIndex)) match {
        case Failure(_) => testSet
        case Success(_) =>
          val horizontalExtensionResult = horizontalExtension(testSet, piList, 0)
          val (horizontalExtendedSet, piRemains) = horizontalExtensionResult.splitAt(currentTestSize)
          val verticalExtendedSet = verticalExtension(horizontalExtendedSet, piRemains)
          extend(verticalExtendedSet, newParamIndex + 1, verticalExtendedSet.length)
      }
    } //End of extend function
    parametersNum - t match {
      case 0 => (sortedParameters, testSet)
      case _ => (sortedParameters, extend(testSet, t, originalTestSize))
    }
  } //End of ipog function
}