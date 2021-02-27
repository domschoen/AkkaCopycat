package models
import akka.event.LoggingAdapter

object Utilities {

  // Same as WorkspaceFormulas.select_list_position
  def valueProportionalRandomIndexInValueList(log: LoggingAdapter, values: List[Double]): Int = {
    val sum = values.sum
    val chosen = Random.rnd(null) * sum

    log.debug(s"sum $sum chosen $chosen")

    valueProportionalIndexInValueListAtValue(0.0, 0, values, chosen)
  }
  def valueProportionalIndexInValueListAtValue(valueAcc: Double, index: Int, values: List[Double], value: Double): Int = {
    val nextValue = values.head
    val newValueAcc = valueAcc + nextValue
    //System.out.println("Choose codelet at index " + index + " urgsum " + newValueAcc);

    if (newValueAcc >= value) {
      index
    } else {
      valueProportionalIndexInValueListAtValue(newValueAcc, index + 1, values.tail, value)
    }
  }



}
