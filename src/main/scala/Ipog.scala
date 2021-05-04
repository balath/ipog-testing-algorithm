import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

object Ipog {
  case class Parameter(name: String,dimension: Int)

  def combineParameters(m: Int, n: Int): Vector[Vector[Int]] = {
    @tailrec
    def combine(lastCombination: Vector[Int], acc: Vector[Vector[Int]]): Vector[Vector[Int]] = {
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
              val newCombination = lastCombination.zipWithIndex.map {
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
        combine (initialCombination, Vector(initialCombination))
      }
    }
  }

  def combineValues(parameters: Vector[Parameter], combination: Vector[Int]): Vector[Vector[Int]] = {
    @tailrec
    def combine(parameters: Vector[Int], acc: Vector[Vector[Int]]):Vector[Vector[Int]] = Try(parameters.head) match {
      case Failure(_) => acc
      case Success(maxDimension) =>  {
        val newVector = for {
          accElem <- acc
          newPElem <- 0 to maxDimension
        } yield accElem :+ newPElem
        combine(parameters.tail,newVector)
      }
    }
    val dimensions = (parameters zip combination).filter(_._2 == 1).map{case (Parameter(_,dimension),_) => dimension - 1}
    combine(dimensions,Vector(Vector.empty))
  }

  def ipog(parameters: Vector[Parameter], t: Int):(Vector[Parameter],Vector[Vector[Option[Int]]]) = {
    val ordParameters = parameters.sortBy(_.dimension)(Ordering.Int.reverse)
    val parametersNum = parameters.length
    val combinations = Vector.fill(t)(1) concat Vector.fill(parametersNum - t)(0)
    val testSet = combineValues(parameters,combinations)




    parametersNum - t match {
      case 0 => (ordParameters,testSet.map(_.map(Some(_))))
      case m => {

        val pi: Vector[Vector[Int]] = combineParameters(m, t)
          .filter(_(m - 1) == 1)
          .flatMap(combination => combineValues(ordParameters,combination))
        (Vector.empty,Vector(Vector.empty))
      }
    }






  }

}
