import scala.annotation.tailrec

object Utils {
  def factorial(n: Long): Long = {
    @tailrec
    def factorialAccumulator(acc: Long, n: Long): Long = {
      if (n == 0) acc
      else factorialAccumulator(n*acc, n-1)
    }
    factorialAccumulator(1, n)
  }

  val nCr = (n: Int, r: Int) => factorial(n) / (factorial(r) * factorial(n-r))

}
