import matrix.RegularMatrix

object Main extends App {
  val m = RegularMatrix(
    List(List(1.0, 2.0, 3.0),
      List(4.0, 5.0, 6.0),
      List(7.0, 8.0, 9.0)))

  println(m.***(m))

  println(m.transpose)
}
