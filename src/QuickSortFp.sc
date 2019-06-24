// https://alvinalexander.com/scala/scala-quicksort-algorithms-fp-recursive-imperative-performance

//object QuickSortFP extends App {

  // create an array of random 10m random ints
  val r = scala.util.Random
  val randomArray = (for (i <- 1 to 100) yield r.nextInt(10000)).toArray

  // do the sorting
  val sortedArray = quickSort(randomArray)
println(sortedArray.mkString(", "))

  // the fp/recursive algorithm
  def quickSort(xs: Array[Int]): Array[Int] = {
    if (xs.length <= 1) xs
    else {
      val pivot = xs(xs.length / 2)
      Array.concat(
        quickSort(xs filter (pivot >)),
        xs filter (pivot ==),
        quickSort(xs filter (pivot <)))
    }
  }

//}
/*
* The FP algorithm took about 16 seconds to complete, while the built-in quickSort function took 4 seconds.
*
* Functional algorithms that run in parallel can be faster than non-FP algorithms.

*/