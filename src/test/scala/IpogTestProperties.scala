import ActsParser._
import Ipog.{Parameter, ipog}
import com.typesafe.scalalogging.Logger
import munit.ScalaCheckSuite
import org.scalacheck.Prop.{forAll,propBoolean}
import org.scalacheck.{Gen, Shrink
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
  val maxT = 5

  implicit val noShrinkInt: Shrink[Int] = Shrink.shrinkAny
  implicit val noShrinkComb: Shrink[List[Int]] = Shrink.shrinkAny

  val genT = Gen.choose(2,maxT)
  val genDimension = Gen.choose(1,maxDimensions)
  val genDimensions = Gen.containerOfN[List,Int](Random.between(1,maxParameters + 1),genDimension)

  property("Generated test set with ipog implementation should be equal to ACTS one") {
    forAll(genDimensions,genT){(dimensions, t) =>
      (t <= dimensions.length) ==> {
        val parameters = dimensions.zipWithIndex.map { case (dim, index) => Parameter(s"P${index + 1}", dim) }
        val inputFileName = s"$outputPath${System.currentTimeMillis}-inputTestSet.txt"
        val (outputParameters, testSet) = ipog(parameters, t)
        val testSetString = testSetToActsInputFormat(outputParameters, testSet)
        writeACTS(testSetString, inputFileName)

        val outputFileName = inputFileName.replace("in", "out")
        val _ = s"java -jar -Dmode=extend -Doutput=csv -Drandstar=off -Dcheck=on -Ddoi=$t ./lib/acts_cmd_2.92.jar ActsConsoleManager $inputFileName $outputFileName" !!

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
