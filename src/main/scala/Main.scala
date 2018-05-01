import matrix.RegularMatrix

object Main extends App {
  val m = RegularMatrix(
    List(List(1, 2, 3),
      List(4, 5, 6),
      List(7, 8, 9)))

  println(m.identityMatrix)
}
