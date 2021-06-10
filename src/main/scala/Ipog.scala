import IpogTypes._
import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

object Ipog {

  def combineParameters(m: Int, n: Int): Vector[ParamComb] = {
    @tailrec
    def combine(lastCombination: ParamComb, acc: Vector[ParamComb]): Vector[ParamComb] = {
      (lastCombination zip lastCombination.tail).lastIndexOf((0, 1)) match {
        case -1 => acc
        case g => {
          lastCombination.last match {
            case 1 => {
              val newCombination = lastCombination.updated(g, 1).updated(g + 1, 0)
              combine(newCombination, acc :+ newCombination)
            }
            case 0 => {
              val r = lastCombination.take(g + 1).count(_ == 1)
              val lastOnes = n - r - 1
              val newCombination = lastCombination.zipWithIndex.map {
                case (_, index) if index >= m - lastOnes => 1
                case (_, index) if index > g => 0
                case (_, index) if index == g => 1
                case (num, _) => num
              }
              combine(newCombination, acc :+ newCombination)
            }
          }
        }
      }
    }

    m match {
      case 0 => Vector.empty
      case _ => {
        val initialCombination = Vector.fill(m - n)(0) ++ Vector.fill(n)(1)
        combine(initialCombination, Vector(initialCombination))
      }
    }
  }

  def combineValues(parameters: Vector[Parameter], combination: ParamComb): Vector[ValuesComb] = {
    @tailrec
    def combine(parameters: ParamComb, acc: Vector[ValuesComb]): Vector[ValuesComb] = parameters match {
      case Vector() => acc
      case dimension +: tail => {
        val newVector = for {
          accElem <- acc
          newElem <- 0 until dimension
        } yield accElem :+ Some(newElem)
        combine(tail, newVector)
      }
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
      coveredValues = values.filter(_.equivTo(matchedParametersValues))
      matches = coveredValues.length
    } yield (value, matches, parameters -> coveredValues)

    val (maxValue, (_, coveredCombinations)) = valueAndCoveredCombinations
      .groupMapReduce(_._1)(t => (t._2, Map(t._3)))((cc1, cc2) => (cc1._1 + cc2._1, cc1._2 ++ cc2._2))
      .maxBy(_._2._1)

    (maxValue, coveredCombinations)
  }

  /**
   * Se consumen los "restos" de la lista Pi, comprobando si hay coincidencia con alguna combinación previa.
   */
  @tailrec
  def verticalExtension(testSet: Vector[ValuesComb], piLeftovers: Vector[ValuesComb]): Vector[ValuesComb] =
    piLeftovers match {
      case Vector() => testSet
      case head +: tail => {
        testSet.indexWhere(_.equivTo(head)) match {
          case -1 => verticalExtension(testSet :+ head, tail)
          case n => verticalExtension(testSet.updated(n, testSet(n).replaceWildcards(head)), tail)
        }
      }
    }

  def ipog(parameters: Vector[Parameter], t: Int): (Vector[Parameter], Vector[ValuesComb]) = {
    val sortedParameters = parameters.sortBy(_.dimension)(Ordering.Int.reverse)
    val parametersNum = parameters.length
    val combinations = Vector.fill(t)(1) concat Vector.fill(parametersNum - t)(0)
    val testSet = combineValues(sortedParameters, combinations)
    val originalTestSize = testSet.length

    /*
     * Función anidada extend
     */
    @tailrec
    def extend(testSet: Vector[ValuesComb], newParamIndex: Int, currentTestSize: Int): Vector[ValuesComb] = {
      def horizontalExtension(testSet: Vector[ValuesComb], piList: PiList, iter: Int): Vector[ValuesComb] = testSet match {
        case Vector() => piList.getLeftovers
        case head +: tail if iter < originalTestSize => {
          val (newValue, coveredValues) = maxCoverageValue(sortedParameters(newParamIndex), head, piList)
          val updatedPiList = piList.updatePi(coveredValues)
          val newRow = head :+ Some(newValue)
          newRow +: horizontalExtension(tail, updatedPiList, iter + 1)
        }
        case head +: tail => {
          val newRow = head :+ Some(0)
          newRow +: horizontalExtension(tail, piList, iter + 1)
        }
      }

      val piList = (for {
        parametersComb <- combineParameters(newParamIndex + 1, t)
        if parametersComb(newParamIndex) == 1
      } yield parametersComb -> combineValues(sortedParameters, parametersComb)).toMap

      Try(sortedParameters(newParamIndex)) match {
        case Failure(_) => testSet
        case Success(_) => {
          val horizontalExtensionResult = horizontalExtension(testSet, piList, 0)
          val (horizontalExtendedSet, piLeftovers) = horizontalExtensionResult.splitAt(currentTestSize)
          val verticalExtendedSet = verticalExtension(horizontalExtendedSet, piLeftovers)
          extend(verticalExtendedSet, newParamIndex + 1, verticalExtendedSet.length)
        }
      }
    } //Fin de función anidada extend
    parametersNum - t match {
      case 0 => (sortedParameters, testSet)
      case _ => (sortedParameters, extend(testSet, t, originalTestSize))
    }
  } //Fin de función ipog
}