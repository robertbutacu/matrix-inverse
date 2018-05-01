import inverse.finder.{InverseFinder, Precision}
import matrix.RegularMatrix
import inverse.finder.InverseApproximator.schultz

object Main extends App {
  val m = RegularMatrix(
    List(List(1.0, 2.0, 3.0, 4.0, 5.0),
      List(6.0, 7.0, 8.0, 9.0, 10.0),
      List(11.0, 12.0, 13.0, 14.0, 15.0),
      List(16.0, 17.0, 18.0, 19.0, 20.0),
      List(21.0, 22.0, 23.0, 24.0, 25.0)))

  val n = RegularMatrix(
    List(List(1.0, 2.0, 3.0), List(4.0, 5.0, 6.0), List(7.0, 8.0, 9.0))
  )

  val nInverse = InverseFinder.find(n, 10000, Precision(5))

  println(n.***(nInverse.get))
}
