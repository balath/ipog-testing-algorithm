import ActsParser._
import IpogD.ipogD
import IpogTypes.Parameter
import com.typesafe.scalalogging.Logger
import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop.{forAll, propBoolean}

import scala.io.Source
import scala.io.AnsiColor.{RED, RESET}
import scala.language.postfixOps
import scala.sys.process._
import scala.util.Random

class IpogDDiferentialTest extends ScalaCheckSuite {
  val logger = Logger(s"${RED}IPOG-D Dif. Test")

  val osName = System.getProperty("os.name")
  val outputPath = if (osName.toLowerCase.contains("windows")) ".\\src\\test\\" else "./src/test/"

  val maxParameters = 10
  val maxDimensions = 4
  val maxT = 6

  /* Parameters generators */
  val genT = Gen.choose(3, maxT)
  val genDimension = Gen.choose(3, maxDimensions)
  val genDimensions = Gen.containerOfN[Vector, Int](Random.between(4, maxParameters + 1), genDimension)

  /* Property test */
  property("Generated test set with ipog implementation should be equal to ACTS one") {
    forAll(genDimensions, genT) { (dimensions, t) =>
      (t <= dimensions.length) ==> {
        /* Test Set is generated and written as ACTS input file */
        val parameters = dimensions.zipWithIndex.map { case (dim, index) => Parameter(s"P${index + 1}", dim) }
        val timeA = System.currentTimeMillis
        val (outputParameters, testSet) = ipogD(parameters, t)
        val timeB = System.currentTimeMillis
        val inputFileName = s"$outputPath${timeA}-inputTestSet.txt"
        val testSetString = testSetToActsInputFormat(outputParameters, testSet)
        writeACTS(testSetString, inputFileName)

        /* ACTS extended file is generated */
        val outputFileName = inputFileName.replace("in", "out")
        val _ = s"java -jar -Dalgo=ipog_d -Dmode=extend -Doutput=csv -Drandstar=off -Dcheck=on " +
          s"-Ddoi=$t ./lib/acts_cmd_2.92.jar ActsConsoleManager $inputFileName $outputFileName" !!

        /* Extracting number of configurations for ipog test set and ACTS extended set */
        val ipogConfigurations = testSet.length
        val bufferedSource = Source.fromFile(outputFileName)
        val actsConfigurations = bufferedSource
          .getLines()
          .find(_.contains("# Number of configurations:"))
          .getOrElse("0")
          .replace("# Number of configurations:", "")
          .trim
          .toInt
        bufferedSource.close()

        /* Coverage equality is checked before assertion for configure log information */
        val coverageIsOk = ipogConfigurations - actsConfigurations == 0

        logger.info(
          s"Test ${
            if (coverageIsOk) "OK!   "
            else "Failed"
          } ACTS: ${f"$actsConfigurations%5d"} IpogD: ${f"$ipogConfigurations%5d"}  " +
            s"Diff: ${((actsConfigurations - ipogConfigurations) * 100) / actsConfigurations}%" +
            s"\n\t\tParameters: ${parameters.map(p => s"(${p.name}, ${p.dimension})").mkString(", ")} t: $t" +
            s"\n\t\tIpogD process time: ${timeB-timeA} ms${RESET}"
        )
        assert(true)
      }
    }
  }
}
