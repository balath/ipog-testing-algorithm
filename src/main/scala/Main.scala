import ActsParser._
import Ipog.ipog

import scala.io.Source
import scala.language.postfixOps
import scala.util.{Failure, Success, Try}

object Main extends App {

  execute(args)

  def execute(arguments: Array[String]): Unit = {
    Try(Source.fromFile(arguments(0))) match {
      case Failure(_) => new RuntimeException("Ruta de archivo .csv incorrecta")
      case Success(bufferedSource) => {
        val rows = (for (line <- bufferedSource.getLines()) yield line.split(",").map(_.trim)).toList
        Try(rows.head zip rows.tail.head.map(_.toInt)) match {
          case Failure(_) => new IllegalArgumentException("Formato de números incorrecto")
          case Success(tuples) => {
            val (parameters, t) = parseCsvTuples(tuples)
            val (sortedParameters, testSet) = ipog(parameters, t)
            val testSetToACTSString = testSetToActsInputFormat(sortedParameters, testSet)

            writeACTS(testSetToACTSString, "testSet.txt")
            println(testSetToACTSString)
          }
        }
        bufferedSource.close
      }
    }
  }
}
