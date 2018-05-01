package inverse.finder

import matrix.RegularMatrix

trait InverseApproximator[A] {
  def computeNext(currApproximation: RegularMatrix[A], matrix: RegularMatrix[A]): RegularMatrix[A]
}

object InverseApproximator {
  implicit def schultz[A: Numeric]: InverseApproximator[A] =
    (currApproximation: RegularMatrix[A], matrix: RegularMatrix[A]) => {
      // V(k+1) = V(k)(2 In - AV(k))
      currApproximation
    }

  implicit def li[A: Numeric]: InverseApproximator[A] =
    (currApproximation: RegularMatrix[A], matrix: RegularMatrix[A]) => {
    currApproximation
  }

  implicit def li2[A: Numeric]: InverseApproximator[A] =
    (currApproximation: RegularMatrix[A], matrix: RegularMatrix[A]) => {
    currApproximation
  }
}
