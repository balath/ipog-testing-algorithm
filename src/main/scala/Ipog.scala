import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

object Ipog {

  case class Parameter(name: String,dimension: Int)
  type Combination = List[Int]
  type OptCombination = List[Option[Int]]
  type PiList = Map[Combination, List[OptCombination]]

  def combineParameters(m: Int, n: Int): List[Combination] = {
    @tailrec
    def combine(lastCombination: Combination, acc: List[Combination]): List[Combination] = {
      (lastCombination zip lastCombination.tail) lastIndexOf ((0, 1)) match {
        case -1 => acc
        case g => {
          lastCombination.last match {
            case 1 => {
              val newCombination = lastCombination.updated(g, 1).updated(g + 1, 0)
              combine(newCombination, acc.appended(newCombination))
            }
            case 0 => {
              val r = lastCombination take (g + 1) count (_ == 1)
              val lastOnes = n - r - 1
              val newCombination = lastCombination.lazyZip(0 to m).map {
                case (_, index) if index >= m - lastOnes => 1
                case (_, index) if index > g => 0
                case (_, index) if index == g => 1
                case (elem, _) => elem
              }
              combine(newCombination, acc.appended(newCombination))
            }
          }
        }
      }
    }
    m match {
      case 0 => List.empty
      case _ => {
        val initialCombination = List.fill(m - n)(0) concat List.fill(n)(1)
        combine(initialCombination, List(initialCombination))
      }
    }
  }

  def combineValues(parameters: List[Parameter], combination: Combination): List[OptCombination] = {
    @tailrec
    def combine(parameters: Combination, acc: List[OptCombination]):List[OptCombination] = parameters match {
      case Nil => acc
      case maxDimension::tail =>  {
        val newList = for {
          accElem <- acc
          newElem <- 0 to maxDimension
        } yield accElem :+ Some(newElem)
        combine(tail,newList)
      }
    }
    val dimensions = (parameters zip combination).filter(_._2 == 1).map{case (Parameter(_,dimension),_) => dimension - 1}
    combine(dimensions,List(List.empty))
  }

  def maxCoverageValue(currentParameter: Parameter, currentRow: OptCombination, pi: PiList): (Int,PiList) = {
    val valueAndCoveredCombinations = for {
      value <- 0 to currentParameter.dimension - 1
      (parameters, values) <- pi
      newRow = currentRow :+ Some(value)
      matchedParametersValues = (parameters zip newRow).filter(_._1 == 1).map(_._2)
      coveredValues = values.filter(equalsCombination(_,matchedParametersValues))
      matches = coveredValues.length
    } yield (value, matches, parameters -> coveredValues)

    valueAndCoveredCombinations.foreach{case (value, matches, params) =>}

    val (maxValue,(_, coveredCombinations)) = valueAndCoveredCombinations
      .groupMapReduce(_._1)(tuple => (tuple._2, Map(tuple._3)))((comb1, comb2) => (comb1._1 + comb2._1, comb1._2 ++ comb2._2))
      .maxBy(_._2._1)

    (maxValue, coveredCombinations)
  }

  def equalsCombination(combi1: OptCombination, combi2: OptCombination): Boolean = (combi1,combi2) match {
    case (Nil, Nil) => true
    case (None::_, _) => true && equalsCombination(combi1.tail, combi2.tail)
    case (_, None::_) => true && equalsCombination(combi1.tail, combi2.tail)
    case (Some(n1)::_, Some(n2)::_) => (n1 == n2) && equalsCombination(combi1.tail, combi2.tail)
    case _ => false
  }

  def updatePi(piList: PiList, coveredCombinations: PiList): PiList = for {
    (parameters, values) <- piList
    updatedValues = values.filterNot(coveredCombinations.getOrElse(parameters,List.empty).contains(_))
  } yield parameters -> updatedValues

  def getLeftovers(piList: PiList): List[OptCombination] = {
    def insertWildcards(parameters: Combination, row: OptCombination): OptCombination = parameters match {
      case Nil => List.empty
      case 0::tail => None +: insertWildcards(tail, row)
      case _ => row.head +: insertWildcards(parameters.tail, row.tail)
    }
    //Se "aplanan" y mapean todas las combinaciones de valores de la lista pi con los comodines
    piList.flatMap { case (parameters, values) => values.map(insertWildcards(parameters, _)) }.toList
  }

  /**
   * Se consumen los "restos" de la lista Pi, comprobando si hay coincidencia con alguna combinación previa.
   */
  @tailrec
  def verticalExtend(testSet: List[OptCombination], piLeftovers: List[OptCombination]):List[OptCombination] =
    piLeftovers match {
      case Nil => testSet
      case head::tail => {
        testSet.indexWhere(equalsCombination(_, head)) match {
          case -1 => verticalExtend(testSet :+ head, piLeftovers.tail)
          case n => { //Reemplazo de los comodines por valores cuando hay coincidencia
            val replacedCombination = (testSet(n) lazyZip head).map{
              case (None, None) => None
              case (None, Some(n)) => Some(n)
              case (Some(n), None) => Some(n)
              case (Some(n), Some(_)) => Some(n)
            }
            verticalExtend(testSet.updated(n, replacedCombination), tail)
          }
        }
      }
    }

  def ipog(parameters: List[Parameter], t: Int):(List[Parameter],List[OptCombination]) = {
    val sortedParameters = parameters.sortBy(_.dimension)(Ordering.Int.reverse)
    val parametersNum = parameters.length
    val combinations = List.fill(t)(1) concat List.fill(parametersNum - t)(0)
    val testSet = combineValues(sortedParameters,combinations)
    val originalTestSize = testSet.length
    /*
     * Función anidada extend
     */
    @tailrec
    def extend(testSet: List[OptCombination], newParameterIndex: Int): List[OptCombination] = {
      var piLeftovers: List[OptCombination] = List.empty
      //Función recursiva interna que recorre el juego de pruebas, añadiendo el valor máximo a la fila y actualizando pi
      def horizontalExtend(testSet: List[OptCombination], piList: PiList, parameter: Parameter, iter: Int): List[OptCombination] = testSet match {
          case Nil => {
            piLeftovers = getLeftovers(piList)
            List.empty
          }
          case head::tail if iter < originalTestSize => {
            val (newValue, coveredValues) = maxCoverageValue(parameter, head, piList)
            val updatedPiList = updatePi(piList, coveredValues)
            val newRow = head :+ Some(newValue)
            newRow +: horizontalExtend(tail, updatedPiList, parameter, iter + 1)
          }
          case head::tail => {
            val newRow = head :+ Some(0)
            newRow +: horizontalExtend(tail, piList, parameter, iter + 1)
          }
        }
      val piList = (for {
        parametersComb <- combineParameters(newParameterIndex + 1, t)
        if parametersComb(newParameterIndex) == 1
      } yield parametersComb -> combineValues(sortedParameters, parametersComb)).toMap
      Try(sortedParameters(newParameterIndex)) match {
        case Failure(_) => testSet
        case Success(parameter) => {
          val horizontalExtendedSet = horizontalExtend(testSet, piList, parameter, 0)
          val verticalExtendedSet = verticalExtend(horizontalExtendedSet, piLeftovers)
          extend(verticalExtendedSet, newParameterIndex + 1)
        }
      }
    }//Fin de función anidada extend
    parametersNum - t match {
      case 0 => (sortedParameters,testSet)
      case _ => (sortedParameters, extend(testSet, t))
    }
  }//Fin de función ipog
}