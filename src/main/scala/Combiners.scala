import IpogTypes.{Dimension, ParamComb, Parameter, ValuesComb}
import scala.annotation.tailrec

object Combiners {

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

}
