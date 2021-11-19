import java.io.{BufferedReader, BufferedWriter, FileReader, FileWriter}
import IpogTypes.{Parameter, ValuesComb}
import cats.effect.IO
import cats.effect.Resource
import scala.jdk.CollectionConverters.IteratorHasAsScala


object ioUtils {

  lazy val readerException = new RuntimeException("csv data format error(not header present or invalid number format)")

  val parseCsvParameters = (csvData: Vector[String]) => {
    val dataRow = csvData.drop(1).head.split(",").map(_.trim.toInt).toVector
    val (dimensions, t) = dataRow.splitAt(dataRow.length - 1)
    val parameters = dimensions.zip(1 to dimensions.length).map(t => Parameter(s"P${t._2}", t._1))
    (parameters, t.head)
  }

  def readFromCsv(fileName: String): IO[(Vector[Parameter], Int)] =
    Resource
      .fromAutoCloseable(IO(new BufferedReader(new FileReader(fileName))))
      .use(reader =>
        IO(parseCsvParameters(reader.lines().iterator().asScala.toVector))
          .handleErrorWith(t => IO.raiseError(readerException))
      )

  val testSetToActsInputFormat = (parameters: Seq[Parameter], testSet: Seq[ValuesComb]) => {
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
