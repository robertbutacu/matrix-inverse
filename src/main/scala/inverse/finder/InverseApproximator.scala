package inverse.finder

import matrix.RegularMatrix

trait InverseApproximator[A] {
  def computeNext(currApproximation: RegularMatrix[A], matrix: RegularMatrix[A]): RegularMatrix[A]
}

object InverseApproximator {
  implicit def schultz[A: Fractional]: InverseApproximator[A] =
    (currApproximation: RegularMatrix[A], matrix: RegularMatrix[A]) => {
      // V(k+1) = V(k)(2 In - AV(k))
      val num = implicitly[Fractional[A]]
      val result =
        currApproximation.***(
          matrix.identityMatrix.map(num.times(_, num.fromInt(2))).---(matrix.***(currApproximation)))

      RegularMatrix(result.rows)
    }

  implicit def li[A: Fractional]: InverseApproximator[A] =
    (currApproximation: RegularMatrix[A], matrix: RegularMatrix[A]) => {
      //V(k+1) = V(k)(3In - AV(k)(3* In- AV(k))
      val num = implicitly[Fractional[A]]

      val identity = matrix.identityMatrix.map(num.times(_, num.fromInt(3)))

      val AtimesVk = matrix.***(currApproximation)
      val identityMinusAtimesVk = identity.---(AtimesVk)
      val secondPartOfEquation = AtimesVk.***(identityMinusAtimesVk)

      val parenthesesResult = identity.---(secondPartOfEquation)
      val result = currApproximation.***(parenthesesResult)

      RegularMatrix(result.rows)
    }

  implicit def li2[A: Fractional]: InverseApproximator[A] =
    (currApproximation: RegularMatrix[A], matrix: RegularMatrix[A]) => {
      //V(k + 1) = (In + 1/4(In - V(k)A) (3*In - V(k)A) ^ 2 )V(k)
      val num = implicitly[Fractional[A]]

      val identity = matrix.identityMatrix

      val vKTimesA = currApproximation.***(matrix)
      val identityMinusVkTimesA = identity.---(vKTimesA)

      val identityTimes3MinusVkTimesA = identity.map(num.times(_, num.fromInt(3))).---(vKTimesA)

      val identityTimes3MinusVkTimesASquared = identityTimes3MinusVkTimesA.***(identityTimes3MinusVkTimesA)

      val parenthesesResult = identityMinusVkTimesA.***(identityTimes3MinusVkTimesASquared)

      val fourthPartOfParentheses = parenthesesResult.map(v => num.div(v, num.fromInt(4)))

      val wholeParenthesesResult = identity.+++(fourthPartOfParentheses)

      val result = currApproximation.***(wholeParenthesesResult)
      RegularMatrix(result.rows)
    }
}
