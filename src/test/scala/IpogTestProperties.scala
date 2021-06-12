import ActsParser._
import Ipog.{Parameter, ipog}
import com.typesafe.scalalogging.Logger
import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop.{forAll, propBoolean}
import scala.io.Source
import scala.language.postfixOps
import scala.sys.process._
import scala.util.Random

class IpogDiferentialTest extends ScalaCheckSuite {
  val logger = Logger("Dif. Test")

  val osName = System.getProperty("os.name")
  val outputPath = if (osName.toLowerCase.contains("windows")) ".\\src\\test\\" else "./src/test/"

  val maxParameters = 10
  val maxDimensions = 5
  val maxT = 4

  /* Parameters generators */
  val genT = Gen.choose(2,maxT)
  val genDimension = Gen.choose(3,maxDimensions)
  val genDimensions = Gen.containerOfN[Vector,Int](Random.between(4,maxParameters + 1),genDimension)

  /* Property test */
  property("Generated test set with ipog implementation should be equal to ACTS one") {
    forAll(genDimensions,genT){(dimensions, t) =>
      (t <= dimensions.length) ==> {
        /* Test Set is generated and written as ACTS input file */
        val parameters = dimensions.zipWithIndex.map { case (dim, index) => Parameter(s"P${index + 1}", dim) }
        val inputFileName = s"$outputPath${System.currentTimeMillis}-inputTestSet.txt"
        val (outputParameters, testSet) = ipog(parameters, t)
        val testSetString = testSetToActsInputFormat(outputParameters, testSet)
        writeACTS(testSetString, inputFileName)

        /* ACTS extended file is generated */
        val outputFileName = inputFileName.replace("in", "out")
        val _ = s"java -jar -Dmode=extend -Doutput=csv -Drandstar=off -Dcheck=on " +
          s"-Ddoi=$t ./lib/acts_cmd_2.92.jar ActsConsoleManager $inputFileName $outputFileName" !!

        /* Extracting number of configurations for ipog test set and ACTS extended set */
        val ipogCongifurations = testSet.length
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
        val coverageIsOk = ipogCongifurations - actsConfigurations == 0

        logger.info(
          s"Test ${
            if (coverageIsOk) "OK!"
            else "Failed"
          } ACTS: $actsConfigurations Ipog: $ipogCongifurations"
        )
        assert(coverageIsOk)
      }
    }
  }
}
