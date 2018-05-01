package matrix

trait Matrix[A] {
  def rows: List[List[A]]
  def rowLength: Int

  def +++(other: Matrix[A])(implicit n: Fractional[A]): Matrix[A]
  def ---(other: Matrix[A])(implicit n: Fractional[A]): Matrix[A]
  def ***(other: Matrix[A])(implicit n: Fractional[A]): Matrix[A]

  def map[B](f: A => B)(implicit n: Fractional[B]): Matrix[B]
  def mapRows[B](f: List[A] => List[B])(implicit n: Fractional[B]): Matrix[B]

  def identityMatrix: Matrix[A]
  def transpose: Matrix[A]
}
