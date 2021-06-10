object IpogTypes {

  case class Parameter(name: String, dimension: Dimension)

  type Dimension = Int
  type ParamComb = Vector[Int]
  type ValuesComb = Vector[Option[Int]]
  type PiList = Map[ParamComb, Vector[ValuesComb]]

  implicit class ValuesCombinationFunctions(comb1: ValuesComb) {
    val equivTo: ValuesComb => Boolean = (comb2: ValuesComb) => (comb1, comb2) match {
      case (Vector(), Vector()) => true
      case (None +: c1tail, _ +: c2tail) => c1tail.equivTo(c2tail)
      case (_ +: c1tail, None +: c2tail) => c1tail.equivTo(c2tail)
      case (Some(n1) +: c1tail, Some(n2) +: c2tail) => (n1 == n2) && c1tail.equivTo(c2tail)
      case _ => false
    }

    val replaceWildcards = (comb2: ValuesComb) => (comb1 lazyZip comb2).map {
      case (None, None) => None
      case (None, Some(n)) => Some(n)
      case (Some(n), None) => Some(n)
      case (Some(n), Some(_)) => Some(n)
    }
  }

  implicit class PiListFunctions(piList: PiList) {
    val updatePi = (coveredCombinations: PiList) => for {
      (parameters, values) <- piList
      updatedValues = values.filterNot(coveredCombinations.getOrElse(parameters, List.empty).contains(_))
    } yield parameters -> updatedValues

    val getLeftovers = {
      def insertWildcards(parameters: ParamComb, row: ValuesComb): ValuesComb = parameters match {
        case Vector() => Vector.empty
        case 0 +: tail => None +: insertWildcards(tail, row)
        case _ +: tail => row.head +: insertWildcards(tail, row.tail)
      }

      piList.flatMap { case (parameters, values) => values.map(insertWildcards(parameters, _)) }.toVector
    }

  }

}
