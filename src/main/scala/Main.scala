import ActsParser._
import Ipog.{Parameter, ipog}

import scala.io.Source
import scala.language.postfixOps
import scala.sys.process._
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
          val testSetString = testSetToActsInputFormat(sortedParameters, testSet)
          val inputFileName = s"testSet-${System.currentTimeMillis()}-in.txt"
          val outputFileName = inputFileName.replace("in","out")
          writeACTS(testSetString,inputFileName)
          println(testSetString)
          val _ = s"java -jar -Dmode=extend -Doutput=csv -Drandstar=off -Dcheck=on -Ddoi=" +
          s"$t ./lib/acts_cmd_2.92.jar ActsConsoleManager $inputFileName $outputFileName" !!
        }
      }
      bufferedSource.close
    }
  }

  def parseCsvTuples(tuples: Array[(String, Int)]): (List[Parameter], Int) = {
    val (parametersTuples, tTuple) = tuples.span(!_._1.startsWith("t"))
    val parameters = parametersTuples.map(tuple => Parameter(tuple._1, tuple._2)).toList
    val t = tTuple.head._2
    (parameters, t)
  }
  //TODO Clases de equivalencia para pruebas unitarias
  //TODO Generar juegos de pruebas con los parámetros
  //TODO Implementarlas pruebas unitarias
}
