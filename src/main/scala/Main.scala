import inverse.finder.{InverseFinder, Precision}
import matrix.RegularMatrix
import inverse.finder.InverseApproximator.li2

object Main extends App {
  val n = RegularMatrix(
    List(List(0.0, 2.0, -1.0), List(3.0, -2.0, 1.0), List(3.0, 2.0, 1.0))
  )

  val result = RegularMatrix(n.***(InverseFinder.find(n, 1000, Precision(4)).get).---(n.identityMatrix).rows)

  println(RegularMatrix.maximumAbsoluteColumnSumNorm(result))
}
