package models

object Utilities {
  val r = scala.util.Random

  def valueProportionalRandomIndexInValueList(values: List[Double]): Int = {
    val sum = values.sum
    val chosen = r.nextDouble() * sum

    valueProportionalIndexInValueListAtValue(0.0, 0, values, chosen)
  }
  def valueProportionalIndexInValueListAtValue(valueAcc: Double, index: Int, values: List[Double], value: Double): Int = {
    val nextValue = values.head
    val newValueAcc = valueAcc + nextValue
    if (newValueAcc >= value) {
      index
    } else {
      valueProportionalIndexInValueListAtValue(newValueAcc, index + 1, values.tail, value)
    }
  }



}
