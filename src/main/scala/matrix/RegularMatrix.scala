package matrix

case class RegularMatrix[A: Fractional](rows: List[List[A]]) extends Matrix[A] {
  require(this.rows.forall(r => r.length == this.rows.maxBy(_.length).length))

  override def rowLength: Int = this.rows.head.length

  override def +++(other: Matrix[A])(implicit n: Fractional[A]): Matrix[A] = {
    require(this.rowLength == other.rowLength)

    applyOperation(other, n.plus)
  }

  override def ---(other: Matrix[A])(implicit n: Fractional[A]): Matrix[A] = {
    require(this.rowLength == other.rowLength)

    applyOperation(other, n.minus)
  }

  private def applyOperation(other: Matrix[A], f: (A, A) => A): Matrix[A] = {
    RegularMatrix[A](this.rows, other, f)
  }

  override def map[B](f: A => B)(implicit n: Fractional[B]): RegularMatrix[B] = RegularMatrix(this.rows.map(_.map(f)))(n)

  override def mapRows[B](f: List[A] => List[B])(implicit n: Fractional[B]): RegularMatrix[B] = RegularMatrix(this.rows.map(f))(n)

  override def identityMatrix: Matrix[A] = {
    val n = this.rows.length
    val m = this.rowLength
    val num = implicitly[Fractional[A]]

    //n rows with 1 on diagonal
    RegularMatrix[A]((0 until n).map(row => List.fill(n)(0).zipWithIndex.map { v =>
      if (v._2 == row) num.fromInt(1)
      else num.zero
    }).toList)
  }

  override def ***(other: Matrix[A])(implicit n: Fractional[A]): Matrix[A] = {
    require(other.rowLength == this.rows.length)
    type ValueWithIndex = (A, Int)

    def updateSum(sumSoFar: A, currElement: ValueWithIndex, currIndex: Int): A =
      n.plus(sumSoFar, n.times(currElement._1, other.rows(currElement._2)(currIndex)))

    def splitIntoRows(values: List[A], rowLength: Int): List[List[A]] = {
      require(values.length % rowLength == 0)

      if (values.isEmpty)
        List.empty
      else
        values.slice(0, rowLength) :: splitIntoRows(values.drop(rowLength), rowLength)
    }

    def computeUpdatedValueForCurrentPosition(l: List[(A, Int)], currIndex: Int) =
      l.foldRight(n.zero) {
        (curr, acc) =>
          updateSum(acc, curr, currIndex)
      }

    val productStream: List[A] = for {
      row <- this.rows
      currIndex <- row.indices.toList
      elementsWithIndex = row.zipWithIndex
      valueForCurrentPosition = computeUpdatedValueForCurrentPosition(elementsWithIndex, currIndex)
    } yield valueForCurrentPosition

    RegularMatrix[A](splitIntoRows(productStream, other.rows.head.length))(n)
  }

  override def transpose: Matrix[A] = {
    val flattenOut = this.rows.zipWithIndex.flatMap(r => r._1.zipWithIndex.map(v => (v._1, v._2)))

    val grouped = flattenOut.groupBy(_._2)
      .values
      .toList
      .sortBy(_.head._2)
      .map(r => r.map(_._1))

    RegularMatrix(grouped)
  }
}

object RegularMatrix {
  def apply[A](rows: List[List[A]],
               other: Matrix[A],
               f: (A, A) => A)(implicit n: Fractional[A]): RegularMatrix[A] =
    new RegularMatrix(rows
      .zip(other.rows)
      .map {
        p =>
          p._1.zip(p._2)
            .map(v => f(v._1, v._2))
      }
    )(n)

  def maximumAbsoluteColumnSumNorm[A: Fractional](matrix: RegularMatrix[A]): A = {
    val matrixAsColumns = matrix.rows.zipWithIndex.flatMap(r => r._1.zipWithIndex.map( v => (v._1, r._2)))
      .groupBy(_._2).values.toList.map(c => c.map(_._1))

    val sums = matrixAsColumns.map(c => c.sum)

    sums.max
  }

  def maximumAbsoluteRowSumNorm[A: Fractional](matrix: RegularMatrix[A]): A = {
    val sums = matrix.rows.map(r => r.sum)

    sums.max
  }
}