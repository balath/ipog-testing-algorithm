import java.io.{File, FileWriter}
import IpogTypes.{Parameter, ValuesComb}


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

  def parseCsvTuples(tuples: Array[(String, Int)]): (Vector[Parameter], Int) = {
    val (parametersTuples, tTuple) = tuples.span(!_._1.startsWith("t"))
    val parameters = parametersTuples.map(tuple => Parameter(tuple._1, tuple._2)).toVector
    val t = tTuple.head._2
    (parameters, t)
  }
}
