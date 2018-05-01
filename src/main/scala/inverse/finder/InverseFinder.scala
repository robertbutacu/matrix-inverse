package inverse.finder

import matrix.RegularMatrix

object InverseFinder {
  def find[A: Numeric](matrix: RegularMatrix[A],
                       kmax: Int,
                       epsilon: Precision)(implicit method: InverseApproximator[A]): Option[RegularMatrix[A]] = {
    def hasReachedEnd(curr: RegularMatrix[A], next: RegularMatrix[A]): Boolean = {
      val frac = implicitly[Fractional[A]]

      curr.rows.zip(next.rows).forall{p =>
        p._1.zip(p._2).forall(v =>
          epsilon.precision <= frac.toDouble(frac.abs(frac.minus(v._1, v._2))))
      }
    }

    def go(currIteration: RegularMatrix[A], k: Int): Option[RegularMatrix[A]] = {
      if (k == kmax) {
        None
      }
      else {
        val nextIteration = method.computeNext(currIteration, matrix)

        if(hasReachedEnd(currIteration, nextIteration)) Some(nextIteration)
        else go(nextIteration, k + 1)
      }
    }

    go(matrix, 0)
  }
}
