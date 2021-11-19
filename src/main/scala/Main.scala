import Ipog.ipog
import cats.effect.{ExitCode, IO, IOApp}
import ioUtils._

object Main extends IOApp {

  override def run(args: List[String]): IO[ExitCode] =
    for {
      dataTuple <- ioUtils.readFromCsv(args(0))

      (parametersDimension, t) = dataTuple
      (sortedParameters, testSet) = ipog(parametersDimension, t)
      testSetInActsFormat = testSetToActsInputFormat(sortedParameters, testSet)

      _ <- writeActsFile(testSetInActsFormat, "testSet.txt") >> IO.println(testSetInActsFormat)
    } yield ExitCode.Success

}
