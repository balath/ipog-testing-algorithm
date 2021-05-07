import Ipog.{ipog, OptCombination, Parameter}

import scala.io.Source
import scala.util.{Failure, Success, Try}

object Main extends App {

  Try(Source.fromFile(args(0))) match {
    case Failure(_) => new RuntimeException("Ruta de archivo .csv incorrecta")
    case Success(bufferedSource) => {
      val rows = (for (line <- bufferedSource.getLines) yield line.split(",").map(_.trim)).toList
      Try(rows.head zip rows.tail.head.map(_.toInt)) match {
        case Failure(_) => new IllegalArgumentException("Formato de números incorrecto")
        case Success(tuples) => {
          val (parameters, t) = parseCsvTuples(tuples)
          val (sortedParameters, testSet) = ipog(parameters, t)
          println(testSetToString(sortedParameters, testSet))
        }
      }
      bufferedSource.close
    }
  }

  def parseCsvTuples(tuples: Array[(String, Int)]): (Vector[Parameter], Int) = {
    val (parametersTuples, tTuple) = tuples.span(!_._1.startsWith("t"))
    val parameters = parametersTuples.map(tuple => Parameter(tuple._1, tuple._2)).toVector
    val t = tTuple.head._2
    (parameters, t)
  }

  def testSetToString(parameters: Vector[Parameter], testSet: Vector[OptCombination]): String = {
    List(
      "=======================",
      s"${parameters.foldLeft("   ")((z,param) => s"$z${param.name}  ")}",
      "=======================",
      testSet
        .map(comb => s"${comb.map{
              case Some(n) => s"$n"
              case None => "*"
            }.foldLeft("")((z, value) => s"$z   $value")
        }")
        .mkString("\n")
    ).mkString("\n")
  }

}
