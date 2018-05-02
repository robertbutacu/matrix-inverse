import inverse.finder.{InverseFinder, Precision}
import matrix.RegularMatrix
import inverse.finder.InverseApproximator.li2

object Main extends App {
  val n = RegularMatrix(
    List(
      List(1.0, 4.0, 0.0, 0.0),
      List(0.0, 1.0, 4.0, 0.0),
      List(0.0, 0.0, 1.0, 4.0),
      List(0.0, 0.0, 0.0, 1.0),
    )
  )

  val inverse = InverseFinder.find(n, 1000, Precision(5)).get
  val result = RegularMatrix(n.***(inverse).---(n.identityMatrix).rows)

  println(inverse)

  println(RegularMatrix.maximumAbsoluteColumnSumNorm(result))
}
