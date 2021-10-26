import java.io.File

import ActsUtils._
import Ipog.ipog
import IpogTypes.Parameter
import cats.effect.{ExitCode, IO, IOApp}
import kantan.csv._
import kantan.csv.ops._

import scala.language.postfixOps

object Main extends IOApp {

  implicit val dimensionsToParameter: Seq[Int] => Vector[Parameter] = (v: Seq[Int]) => {
    val names = (1 to v.length).map(n => s"P$n").toVector
    names.zip(v).map(p => Parameter(p._1, p._2))
  }

  override def run(args: List[String]): IO[ExitCode] =
    for {
      row <- readFromCsv(args(0))
      (parameters, t)= row.splitAt(row.length - 1)
      (sortedParameters, testSet) = ipog(parameters, t.head)
      testSetInActsFormat = testSetToActsInputFormat(sortedParameters, testSet)
      _ <- IO.println(testSetInActsFormat)
      _ <- IO(writeACTS(testSetInActsFormat, "testSet.txt"))
    } yield ExitCode.Success

  def readFromCsv(fileName: String): IO[Seq[Int]] = {
    val csvData = new File(fileName).readCsv[Vector, Vector[Int]](rfc.withHeader)
    csvData.head match {
      case Left(e) => IO.raiseError(e)
      case Right(row) => if (row.length > 1) IO(row) else IO.raiseError(new IllegalArgumentException("Invalid csv data"))
    }
  }


  //  Extending App (Scala standard library) version:
  //  -------------------------------------------------------
  //  Try(Source.fromFile(args(0))) match {
  //      case Failure(_) => throw new RuntimeException("Ruta de archivo .csv incorrecta")
  //      case Success(bufferedSource) => {
  //        val rows = (for (line <- bufferedSource.getLines()) yield line.split(",").map(_.trim)).toList
  //        Try(rows.head zip rows.tail.head.map(_.toInt)) match {
  //          case Failure(_) => throw new IllegalArgumentException("Formato de números incorrecto")
  //          case Success(tuples) => {
  //            val (parameters, t) = parseCsvTuples(tuples)
  //            val (sortedParameters, testSet) = ipog(parameters, t)
  //            val testSetToACTSString = testSetToActsInputFormat(sortedParameters, testSet)
  //
  //            println(testSetToACTSString)
  //            writeACTS(testSetToACTSString, "testSet.txt")
  //          }
  //        }
  //        bufferedSource.close
  //    }
  //  }
}
