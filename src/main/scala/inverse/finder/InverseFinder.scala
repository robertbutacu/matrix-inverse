package inverse.finder

import matrix.RegularMatrix

object InverseFinder {
  def find[A: Fractional](matrix: RegularMatrix[A],
                       kmax: Int,
                       epsilon: Precision)(implicit method: InverseApproximator[A]): Option[RegularMatrix[A]] = {
    val frac = implicitly[Fractional[A]]

    def go(currIteration: RegularMatrix[A], k: Int): Option[RegularMatrix[A]] = {
      if (k == kmax) {
        None
      }
      else {
        val nextIteration = method.computeNext(currIteration, matrix)

        println(s"""Iteration $k : $currIteration""")
        println(s"""Iteration ${k+1}: $nextIteration \n\n""")

        if(isLowNorm(currIteration, nextIteration, epsilon)) Some(nextIteration)
        else go(nextIteration, k + 1)
      }
    }

    val maxColumnSumNorm = RegularMatrix.maximumAbsoluteColumnSumNorm(matrix)
    val maxRowSumNorm = RegularMatrix.maximumAbsoluteRowSumNorm(matrix)

    val product = frac.times(maxColumnSumNorm, maxRowSumNorm)
    val v0 = RegularMatrix(matrix.transpose.map(v => frac.div(v, product)).rows)

    go(v0, 0)
  }

  def isLowNorm[A: Fractional](curr: RegularMatrix[A],
                               next: RegularMatrix[A],
                               epsilon: Precision): Boolean = {
    val frac = implicitly[Fractional[A]]

    curr.rows.zip(next.rows).forall{p =>
      p._1.zip(p._2).forall(v =>
        epsilon.precision >= frac.toDouble(frac.abs(frac.minus(v._1, v._2))))
    }
  }

  def processResult[A: Fractional](matrix: Option[RegularMatrix[A]]): Unit = {
    matrix match {
      case None => println("Divergenta")
      case Some(m) => println(m)
    }
  }
}
