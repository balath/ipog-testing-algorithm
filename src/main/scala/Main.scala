import Ipog.{ipog, OptCombination, Parameter}

import scala.io.Source
import scala.util.{Failure, Success, Try}

object Main extends App {


  Try(Source.fromFile(args(0))) match {
    case Failure(exception) => throw new RuntimeException("Ruta de archivo .csv incorrecta")
    case Success(bufferedSource) => {
      val lines = (for (line <- bufferedSource.getLines) yield line.split(",").map(_.trim)).toList
      val tuples = Try(lines.head zip lines.tail.head.map(_.toInt))
      tuples match {
        case Failure(_) => new IllegalArgumentException("Formato de números incorrecto")
        case Success(tuples) => {
          val (parametersTuples, tTuple) = tuples.span(!_._1.startsWith("t"))
          val parameters = parametersTuples.map(tuple => Parameter(tuple._1, tuple._2)).toVector
          val t = tTuple.head._2
          val (sortedParameters, testSet) = ipog(parameters, t)
          displayTestSet(sortedParameters, testSet)
        }
          bufferedSource.close
      }
    }
  }

  def displayTestSet(parameters: Vector[Parameter], testSet: Vector[OptCombination]): Unit = {
    println("=======================")
    print("  ")
    parameters.foreach(p => print(s"${p.name}  "))
    println
    println("=======================")
    testSet.foreach(comb => println(comb.map{
      case Some(n) => s"$n"
      case None => "*"
    }.foldLeft("")(_ + "   " + _)))
  }

}
