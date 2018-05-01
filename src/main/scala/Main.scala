import matrix.RegularMatrix

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

  println(RegularMatrix.maximumAbsoluteColumnSumNorm(n))//should be 3 + 6 + 9 = 18
  println(RegularMatrix.maximumAbsoluteRowSumNorm(n))// should be 7 + 8 + 9 = 24
  println(RegularMatrix.maximumAbsoluteRowSumNorm(m))//21 + 22 + 23 + 24 + 25 = 100 + 6 + 6 + 3 = 115
  println(RegularMatrix.maximumAbsoluteColumnSumNorm(m))// 5 + 10 + 15 + 20 + 25 = 40 + 30 + 5 = 75
}
