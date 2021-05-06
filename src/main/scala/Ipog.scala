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
      case Success(maxDimension) =>  {
        val newVector = for {
          accElem <- acc
          newElem <- 0 to maxDimension
        } yield accElem :+ Some(newElem)
        combine(parameters.tail,newVector)
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
    case (Failure(_), Failure(_)) => true
    case (Failure(_), _) => false
    case (_, Failure(_)) => false
    case (Success(None), Success(None)) => true && equalsCombination(combi1.tail, combi2.tail)
    case (Success(None), _) => true && equalsCombination(combi1.tail, combi2.tail)
    case (_, Success(None)) => true && equalsCombination(combi1.tail, combi2.tail)
    case (Success(Some(n1)), Success(Some(n2))) => (n1 == n2) && equalsCombination(combi1.tail, combi2.tail)
  }

  def updatePi(piList: PiList, coveredCombinations: PiList): PiList = for {
    (parameters, values) <- piList
    updatedValues = values.filterNot(coveredCombinations.getOrElse(parameters,Vector.empty).contains(_))
  } yield parameters -> updatedValues

  def verticalExtend(piList: PiList): Vector[OptCombination] = {
    def insertWildcards(parameters: Combination, row: OptCombination): OptCombination = Try(parameters.head) match {
      case Failure(_) => Vector.empty
      case Success(0) => None +: insertWildcards(parameters.tail, row)
      case Success(1) => row.head +: insertWildcards(parameters.tail, row.tail)
    }
    piList.flatMap{case (parameters, values) => values.map(insertWildcards(parameters,_))}.toVector
  }

  def ipog(parameters: Vector[Parameter], t: Int):(Vector[Parameter],Vector[OptCombination]) = {

    val sortedParameters = parameters.sortBy(_.dimension)(Ordering.Int.reverse)
    val parametersNum = parameters.length
    val combinations = Vector.fill(t)(1) concat Vector.fill(parametersNum - t)(0)
    val testSet = combineValues(sortedParameters,combinations)
    @tailrec
    def extend(testSet: Vector[OptCombination], newParameterIndex: Int): Vector[OptCombination] = {
      //Función recursiva interna que recorre el juego de pruebas, añadiendo el valor máximo a la fila y actualizando pi
      def horizontalExtend(testSet: Vector[OptCombination], piList: PiList, parameter: Parameter): Vector[OptCombination] =
        //Se consume y procesa cada fila del juego hasta que se acabe y se devuelva el juego extendido.
        Try(testSet.head) match {
          case Failure(_) => verticalExtend(piList)
          case Success(row) => {
            val (newValue, coveredValues) = maxCoverageValue(parameter, row, piList)
            val updatedPiList = updatePi(piList, coveredValues)
            val newRow = Vector(row :+ Some(newValue))
            newRow ++ horizontalExtend(testSet.tail, updatedPiList, parameter)
          }
        }

      //En cada recursión se crea una lista pi para el nuevo parámetro con el que se va a extender el juego de pruebas
      val piList = (for {
        parametersComb <- combineParameters(newParameterIndex + 1, t)
        if parametersComb(newParameterIndex) == 1
      } yield parametersComb -> combineValues(sortedParameters, parametersComb)).toMap
      //Se intenta acceder al siguiente parámetro, en caso de que no exista se devuelve el juego, si existe se extiende.
      Try(sortedParameters(newParameterIndex)) match {
        case Failure(_) => testSet
        case Success(parameter) => extend(horizontalExtend(testSet, piList, parameter), newParameterIndex + 1)
      }
    }

    parametersNum - t match {
      case 0 => (sortedParameters,testSet)
      case m => (sortedParameters, extend(testSet, m - 1))
    }
  }

}
