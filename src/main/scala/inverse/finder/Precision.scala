package inverse.finder

case class Precision(magnitude: Int) {
  val precision: Double = Math.pow(10, - magnitude)
}
