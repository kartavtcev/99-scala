
def fib (n : Int) : Option[Int] = {
  def loop(i : Int, n1 : Int, n2: Int) : Option[Int] = {
    if (i == n) Some(n1 + n2)
    else if (i == 1) loop(i + 1, 0, 1)
    else if (i == 2) loop(i + 1, 0, 1)
    else if (i < n) loop( i + 1 , n2, n1 + n2)
    else None
  }
  loop(1, 0, 0)
}

for(i <- 1 to 10) print(s"${fib(i).getOrElse(-1)}, ")