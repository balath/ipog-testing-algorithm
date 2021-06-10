import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

object Ipog {

  case class Parameter(name: String,dimension: Int)
  type Combination = Vector[Int]
  type OptCombination = Vector[Option[Int]]
  type PiList = Map[Combination, Vector[OptCombination]]

  def combineParameters(m: Int, n: Int): Vector[Combination] = {
    @tailrec
    def combine(lastCombination: Combination, acc: Vector[Combination]): Vector[Combination] = {
      (lastCombination zip lastCombination.tail) lastIndexOf ((0, 1)) match {
        case -1 => acc
        case g => {
          lastCombination.last match {
            case 1 => {
              val newCombination = lastCombination.updated(g, 1).updated(g + 1, 0)
              combine(newCombination, acc :+ newCombination)
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
              combine(newCombination, acc :+ newCombination)
            }
          }
        }
      }
    }
    m match {
      case 0 => Vector.empty
      case _ => {
        val initialCombination = Vector.fill(m - n)(0) concat Vector.fill(n)(1)
        combine(initialCombination, Vector(initialCombination))
      }
    }
  }

  def combineValues(parameters: Vector[Parameter], combination: Combination): Vector[OptCombination] = {
    @tailrec
    def combine(parameters: Combination, acc: Vector[OptCombination]):Vector[OptCombination] = Try(parameters.head) match {
      case Failure(_) => acc
      case Success(head) =>  {
        val newList = for {
          accElem <- acc
          newElem <- 0 to head
        } yield accElem :+ Some(newElem)
        combine(parameters.tail,newList)
      }
    }
    val dimensions = (parameters zip combination).filter(_._2 == 1).map{case (Parameter(_,dimension),_) => dimension - 1}
    combine(dimensions,Vector(Vector.empty))
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

    val (maxValue,(_, coveredCombinations)) = valueAndCoveredCombinations
      .groupMapReduce(_._1)(tuple => (tuple._2, Map(tuple._3)))((comb1, comb2) => (comb1._1 + comb2._1, comb1._2 ++ comb2._2))
      .maxBy(_._2._1)

    (maxValue, coveredCombinations)
  }

  def equalsCombination(combi1: OptCombination, combi2: OptCombination): Boolean = (Try(combi1.head),Try(combi2.head)) match {
    case (Failure(_),Failure(_)) => true
    case (Success(None), Success(_)) => true && equalsCombination(combi1.tail, combi2.tail)
    case (Success(_), Success(None)) => true && equalsCombination(combi1.tail, combi2.tail)
    case (Success(Some(n1)), Success(Some(n2))) => (n1 == n2) && equalsCombination(combi1.tail, combi2.tail)
    case _ => false
  }

  def updatePi(piList: PiList, coveredCombinations: PiList): PiList = for {
    (parameters, values) <- piList
    updatedValues = values.filterNot(coveredCombinations.getOrElse(parameters,Vector.empty).contains(_))
  } yield parameters -> updatedValues

  def getLeftovers(piList: PiList): Vector[OptCombination] = {
    def insertWildcards(parameters: Combination, row: OptCombination): OptCombination = Try(parameters.head) match {
      case Failure(_) => Vector.empty
      case Success(0) => None +: insertWildcards(parameters.tail, row)
      case _ => row.head +: insertWildcards(parameters.tail, row.tail)
    }
    //Se "aplanan" y mapean todas las combinaciones de valores de la lista pi con los comodines
    piList.flatMap { case (parameters, values) => values.map(insertWildcards(parameters, _)) }.toVector
  }

  /**
   * Se consumen los "restos" de la lista Pi, comprobando si hay coincidencia con alguna combinación previa.
   */
  @tailrec
  def verticalExtension(testSet: Vector[OptCombination], piLeftovers: Vector[OptCombination]):Vector[OptCombination] =
    Try(piLeftovers.head) match {
      case Failure(_) => testSet
      case Success(head) => {
        testSet.indexWhere(equalsCombination(_, head)) match {
          case -1 => verticalExtension(testSet :+ head, piLeftovers.tail)
          case n => { //Reemplazo de los comodines por valores cuando hay coincidencia
            val replacedCombination = (testSet(n) lazyZip head).map{
              case (None, None) => None
              case (None, Some(n)) => Some(n)
              case (Some(n), None) => Some(n)
              case (Some(n), Some(_)) => Some(n)
            }
            verticalExtension(testSet.updated(n, replacedCombination), piLeftovers.tail)
          }
        }
      }
    }

  def ipog(parameters: Vector[Parameter], t: Int):(Vector[Parameter],Vector[OptCombination]) = {
    val sortedParameters = parameters.sortBy(_.dimension)(Ordering.Int.reverse)
    val parametersNum = parameters.length
    val combinations = Vector.fill(t)(1) concat Vector.fill(parametersNum - t)(0)
    val testSet = combineValues(sortedParameters,combinations)
    val originalTestSize = testSet.length
    /*
     * Función anidada extend
     */
    @tailrec
    def extend(testSet: Vector[OptCombination], newParamIndex: Int, currentTestSize: Int): Vector[OptCombination] = {
      var piLeftovers: Vector[OptCombination] = Vector.empty
      //Función recursiva interna que recorre el juego de pruebas, añadiendo el valor máximo a la fila y actualizando pi
      def horizontalExtension(testSet: Vector[OptCombination], piList: PiList, iter: Int): Vector[OptCombination] = Try(testSet.head) match {
        case Failure(_) => getLeftovers(piList)
          case Success(head) if iter < originalTestSize => {
            val (newValue, coveredValues) = maxCoverageValue(sortedParameters(newParamIndex), head, piList)
            val updatedPiList = updatePi(piList, coveredValues)
            val newRow = head :+ Some(newValue)
            newRow +: horizontalExtension(testSet.tail, updatedPiList, iter + 1)
          }
          case Success(head) => {
            val newRow = head :+ Some(0)
            newRow +: horizontalExtension(testSet.tail, piList, iter + 1)
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
    }//Fin de función anidada extend
    parametersNum - t match {
      case 0 => (sortedParameters,testSet)
      case _ => (sortedParameters, extend(testSet, t, originalTestSize))
    }
  }//Fin de función ipog
}