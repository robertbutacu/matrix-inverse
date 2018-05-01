package inverse.finder

import matrix.RegularMatrix

trait InverseApproximator[A] {
  def computeNext(currApproximation: RegularMatrix[A]): RegularMatrix[A]
}

object InverseApproximator{
  implicit def schultz[A: Numeric]: InverseApproximator[A] = (currApproximation: RegularMatrix[A]) => {
    currApproximation
  }

  implicit def li[A: Numeric]: InverseApproximator[A] = (currApproximation: RegularMatrix[A]) => {
    currApproximation
  }

  implicit def li2[A: Numeric]: InverseApproximator[A] = (currApproximation: RegularMatrix[A]) => {
    currApproximation
  }
}
