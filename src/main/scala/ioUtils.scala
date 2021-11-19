import java.io.{BufferedWriter, FileWriter}
import IpogTypes.{Parameter, ValuesComb}
import cats.effect.{IO, Resource}

object ioUtils {

  def parseCsvParameters(csvData: Vector[String]): (Vector[Parameter], Int) = {
    val dataRow = csvData.drop(1).head.split(",").map(_.trim.toInt).toVector
    val (dimensions, t) = dataRow.splitAt(dataRow.length - 1)
    val parameters = dimensions.zip(1 to dimensions.length).map(t => Parameter(s"P${t._2}", t._1))
    (parameters, t.head)
  }

  def readFromCsv(fileName: String): IO[(Vector[Parameter], Int)] =
    Resource
      .fromAutoCloseable(IO(scala.io.Source.fromFile(fileName)))
      .use(reader =>
        IO(parseCsvParameters(reader.getLines.toVector))
          .handleErrorWith(_ => IO.raiseError(
            new RuntimeException("csv data format error(not header present or invalid number format)")
          ))
      )

  def testSetToActsInputFormat(parameters: Seq[Parameter], testSet: Seq[ValuesComb]): String = {
    Vector(
    "[Parameter]",
    s"${parameters.foldLeft("")((z,param) => s"$z${param.name} (int) : ${(0 until param.dimension).mkString(", ")}\n")}",
    "[Test Set]",
    parameters.map(_.name).mkString(", "),
    testSet
        .map(combination => combination.map{
          case Some(n) => s"$n"
          case None => "*"
        }.mkString(", "))
        .mkString("\n")
    ).mkString("\n")
  }

  def writeActsFile(testSetString: String, fileName: String): IO[Unit] =
  Resource
      .fromAutoCloseable(IO(new BufferedWriter(new FileWriter(fileName))))
      .use(writer => IO(writer.write(testSetString)))

}
