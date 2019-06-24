// https://algs4.cs.princeton.edu/20sorting/
// Mutable (on future)
// ArrayBuffer

import util.control.Breaks._

object Sorts { // TODO: <% is deprecated, used implicit parameter
  def sort[T <% Ordered[T]](a: Array[T], lo: Int, hi: Int): Unit = {
    if(hi <= lo) return
    // TODO: finish
    val j: Int = partition(a, lo, hi)
    sort(a, lo, j-1)
    sort(a, j+1, hi)
    // isSorted // assert
  }

  def partition[T <% Ordered[T]](a: Array[T], lo: Int, hi: Int): Int = {
    var i = lo
    var j = hi + 1

    val v = a(lo)

    while(i < j){ // true
      i = i + 1
      breakable {
        while (a(i) < v) { //       while(a(++i) < v) {
          if (i == hi) break
          i = i + 1
        }
      }

      j = j - 1
      breakable {
        while (v < a(j)) { //          while(v < a(--j)) {
          if (j == lo) break
          j = j - 1
        }
      }

      //if(i >= j) break

      if(i < j) {
        exch(a, i, j)
      }
    }

    exch(a, lo, j)

    return j
  }

  def exch[T <% Ordered[T]](a: Array[T], i: Int, j: Int): Unit = {
    val swap = a(i)
    a(i) = a(j)
    a(j) = swap
  }
} // TODO: shuffle

val array = Array[Int](5,7,2,1)
Sorts.sort(array, 0, array.length - 1)

println(array.mkString(", "))