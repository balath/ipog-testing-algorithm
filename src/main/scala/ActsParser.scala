import java.io.{File, FileWriter}

import IpogTypes._

object ActsParser {
  def testSetToActsInputFormat(parameters: Vector[Parameter], testSet: Vector[ValuesComb]): String = {
    Vector(
      "[Parameter]",
      s"${parameters.foldLeft("")((z,param) => s"$z${param.name} (int) : ${(0 until param.dimension).mkString(", ")}\n")}",
      "[Test Set]",
      parameters.map(_.name).mkString(", "),
      testSet
        .map(comb => comb.map{
          case Some(n) => s"$n"
          case None => "*"
        }.mkString(", "))
        .mkString("\n")
    ).mkString("\n")
  }

  def writeACTS(testSetString: String, fileName: String): Unit = {
    val fileWriter = new FileWriter(new File(fileName))
    fileWriter.write(testSetString)
    fileWriter.close()
  }
}
