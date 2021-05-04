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

  def nCr(n: Int, r: Int): Option[Long] = (n,r) match {
    case (n, r) if n == r => Some(1)
    case (_, 0) => Some(1)
    case (0, _) => Some(0)
    case (n,r) if n < 0 || r < 0 || r > n  => None
    case _ => Some(factorial (n) / (factorial (r) * factorial (n - r) ))
  }

}
