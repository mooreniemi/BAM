import scala.util.Sorting
import scala.annotation.tailrec

object Quickselect {
  def sortselect(k: Int, xs: Array[Int]): Option[Int] = {
    Sorting.quickSort(xs)
    xs.lift(k - 1)
  }
  def quickselect(k: Int, xs: Array[Int]): Option[Int] =
    ???
}
