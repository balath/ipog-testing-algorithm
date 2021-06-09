import scala.util.{Failure, Success, Try}

object IpogTypes {

  type Dimension = Int
  case class Parameter(name: String,dimension: Dimension)
  type ParamComb = Vector[Int]
  type ValuesComb = Vector[Option[Int]]
  type PiList = Map[ParamComb, Vector[ValuesComb]]

  /**
   * Clase implícita que enriquece el tipo ValuesComb con funcionalidades
   */
  implicit class ValuesCombinationFunctions(comb1: ValuesComb){

    /* Comprueba la equivalencia de  dos combinaciones */
    val equivTo: ValuesComb => Boolean = (comb2: ValuesComb)  => (Try(comb1.head),Try(comb2.head)) match {
      case (Failure(_), Failure(_)) => true
      case (Success(None), _) => true && comb1.tail.equivTo(comb2.tail)
      case (_, Success(None)) => true && comb1.tail.equivTo(comb2.tail)
      case (Success(n1), Success(n2)) => (n1 == n2) && comb1.tail.equivTo(comb2.tail)
      case _ => false
    }

    /* Reemplaza los comodines por valores en dos combinaciones equivalentes */
    val replaceWildcards = (comb2: ValuesComb) => (comb1 lazyZip comb2).map{
      case (None, None) => None
      case (None, Some(n)) => Some(n)
      case (Some(n), None) => Some(n)
      case (Some(n), Some(_)) => Some(n)
    }
  }

  /**
   * Clase implícita que enriquece el tipo PiList con funcionalidades
   */
  implicit class PiListFunctions(piList: PiList) {
    /**
     * Devuelve una lista Pi actualizada según las combinaciones cubiertas
     */
    val updatePi = (coveredCombinations: PiList) => for {
      (parameters, values) <- piList
      updatedValues = values.filterNot(coveredCombinations.getOrElse(parameters,List.empty).contains(_))
    } yield parameters -> updatedValues

    /**
     * Para obtener las "sobras" se mapean todas las combinaciones de valores de la lista pi con los comodines y
     * se aplana la estructura de la lista Pi a una lista de un solo nivel.
     */
    val getLeftovers = {
      def insertWildcards(parameters: ParamComb, row: ValuesComb): ValuesComb = Try(parameters.head) match {
        case Failure(_) => Vector.empty
        case Success(0) => None +: insertWildcards(parameters.tail, row)
        case _ => row.head +: insertWildcards(parameters.tail, row.tail)
      }
      piList.flatMap { case (parameters, values) => values.map(insertWildcards(parameters, _)) }.toVector
    }
  }

}
