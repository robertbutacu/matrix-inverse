package inverse.finder

import matrix.RegularMatrix

object InverseFinder {
  def find[A: Numeric](matrix: RegularMatrix[A], kmax: Int): Option[RegularMatrix[A]] ={
    def go(matrix: RegularMatrix[A], k: Int): Option[RegularMatrix[A]] = {
      if( k == kmax){
        None
      }
      else {
        Some(matrix)
      }
    }

    go(matrix, 0)
  }
}
