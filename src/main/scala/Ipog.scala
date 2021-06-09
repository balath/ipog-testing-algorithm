import scala.util.{Failure, Success, Try}
import IpogTypes._

object Ipog {

  def combineParameters(m: Int, n: Int): Vector[ParamComb] = {
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
              val last1 = n - r - 1
              val newCombination = lastCombination.zipWithIndex map {
                case (_, index) if index >= m - last1 => 1
                case (_, index) if index > g => 0
                case (_, index) if index == g => 1
                case (num, _) => num
              }
              combine(newCombination, acc :+ newCombination)
            }
          }
        }
      }
    }//Fin de la función anidada combine
    m match {
      case 0 => Vector.empty
      case _ => {
        val initialCombination = Vector.fill(m - n)(0) ++ Vector.fill(n)(1)
        combine(initialCombination, Vector(initialCombination))
      }
    }
  }

  def combineValues(parameters: Vector[Parameter], combination: ParamComb): Vector[ValuesComb] = {
    def combine(dimensions: Vector[Dimension], acc: Vector[ValuesComb]):Vector[ValuesComb] = Try(dimensions.head) match {
      case Failure(_) => acc
      case Success(dimension) =>  {
        val newList = for {
          accElem <- acc
          newElem <- 0 until dimension
        } yield accElem :+ Some(newElem)
        combine(dimensions.tail,newList)
      }
    }//Fin de la función anidada combine
    val dimensions = (parameters zip combination).filter(_._2 == 1).map(_._1.dimension)
    combine(dimensions,Vector(Vector.empty))
  }

  /**
   * Función principal de ipog
   */
  def ipog(parameters: Vector[Parameter], t: Int):(Vector[Parameter],Vector[ValuesComb]) = {
    val sortedParameters = parameters.sortBy(_.dimension)(Ordering.Int.reverse)
    val totalParameters = parameters.length
    val tCombination = Vector.fill(t)(1) ++ Vector.fill(totalParameters - t)(0)
    val testSet = combineValues(sortedParameters,tCombination)
    val originalTestSize = testSet.length
    /* Función anidada que extiende el juego de pruebas */
    def extend(testSet: Vector[ValuesComb], newParamIndex: Int, currentTestSize: Int): Vector[ValuesComb] = {
      var piLeftovers: Vector[ValuesComb] = Vector.empty
      def horizontalExtension(testSet: Vector[ValuesComb], piList: PiList, iter: Int): Vector[ValuesComb] =
      Try(testSet.head) match {
        case Failure(_) => {
          piLeftovers = piList.getLeftovers
          Vector.empty
        }
        case Success(head) if iter < originalTestSize => {
          val (newValue, coveredValues) = maxCoverageValue(parameters(newParamIndex), head, piList)
          val updatedPiList = piList.updatePi(coveredValues)
          val newRow = head :+ Some(newValue)
          newRow +: horizontalExtension(testSet.tail, updatedPiList, iter + 1)
        }
        case Success(head) => (head :+ Some(0)) +: horizontalExtension(testSet.tail, piList, iter + 1)
      } //Fin de horizontalExtension

      lazy val piList = (for {
        parametersComb <- combineParameters(newParamIndex + 1, t)
        if parametersComb(newParamIndex) == 1
      } yield parametersComb -> combineValues(sortedParameters, parametersComb)).toMap

      Try(sortedParameters(newParamIndex)) match {
        case Failure(_) => testSet
        case Success(_) => {
          val horizontalExtendedSet = horizontalExtension(testSet, piList, 0)
          //val (horizontalExtendedSet, piLeftovers) = horizontalExtensionResult.splitAt(currentTestSize)
          val verticalExtendedSet = verticalExtension(horizontalExtendedSet, piLeftovers)
          extend(verticalExtendedSet, newParamIndex + 1, verticalExtendedSet.length)
        }
      }
    }//Fin de función anidada extend
    totalParameters - t match {
      case 0 => (sortedParameters,testSet)
      case _ => (sortedParameters, extend(testSet, t, originalTestSize))
    }
  }//Fin de función ipog

  /**
   * Para cada posible valor del nuevo parámetro, y para cada comb. de parámetros (con sus combs. de valores): Crea
   * una nueva fila con el valor, ajustada a la comb. de parámetros, guarda las comb. de valores cubiertas y el número
   * de combinaciones que cubre. Después, mediante: la agrupación por el nuevo valor usado(group), del número total y
   * comb. cubiertas para cada comb. de parámetros(Map), y la suma de los totales y concatenación de las comb. cubiertas
   * para cada valor(reduce), obteniendo finalmente el valor con mayor cobertura y sus comb.(maxBy(matches)).
   */
  def maxCoverageValue(currentParameter: Parameter, currentRow: ValuesComb, pi: PiList): (Int, PiList) = {
    val valueAndCoveredCombinations = for {
      value <- 0 until currentParameter.dimension
      (parameters, values) <- pi
      newRow = currentRow :+ Some(value)
      newRowActiveParameters = (parameters zip newRow).filter(_._1 == 1).map(_._2)
      coveredValuesCombinations = values.filter(_.equivTo(newRowActiveParameters))
      matches = coveredValuesCombinations.length
    } yield (value, matches, parameters -> coveredValuesCombinations)
    val (maxValue, (_, coveredCombinations)) = valueAndCoveredCombinations
      .groupMapReduce(_._1)(tuple => (tuple._2, Map(tuple._3)))((v1, v2) => (v1._1 + v2._1, v1._2 ++ v2._2))
      .maxBy(_._2._1) //Se accede al primer elemento la tupla anidada => (maxValue, (matches, coveredCombinations))
    (maxValue, coveredCombinations)
  }

  /**
   * Extiende
   */
  def verticalExtension(testSet: Vector[ValuesComb], piLeftovers: Vector[ValuesComb]):Vector[ValuesComb] =
    Try(piLeftovers.head) match {
      case Failure(_) => testSet
      case Success(head) => {
        testSet.zipWithIndex.find(_._1.equivTo(head)) match {
          case None => verticalExtension(testSet :+ head, piLeftovers.tail)
          case Some((comb, n)) => verticalExtension(testSet.updated(n, head.replaceWildcards(comb)), piLeftovers.tail)
        }
      }
    } //Fin de verticalExtension
}
