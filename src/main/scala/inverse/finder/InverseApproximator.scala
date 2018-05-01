package inverse.finder

import matrix.RegularMatrix

trait InverseApproximator[A] {
  def computeNext(currApproximation: RegularMatrix[A], matrix: RegularMatrix[A]): RegularMatrix[A]
}

object InverseApproximator {
  implicit def schultz[A: Numeric]: InverseApproximator[A] =
    (currApproximation: RegularMatrix[A], matrix: RegularMatrix[A]) => {
      // V(k+1) = V(k)(2 In - AV(k))
      val num = implicitly[Numeric[A]]
      val result =
        currApproximation.***(
          matrix.identityMatrix.map(num.times(_, num.fromInt(2))).---(matrix.***(currApproximation)))

      RegularMatrix(result.rows)
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
