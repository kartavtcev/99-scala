import scala.annotation.tailrec

// worst-case space compl O(1), worst-case time compl O(n)
def fib (n : Int) : Option[Int] = {
  @tailrec
  def loop(i : Int, n1 : Int, n2: Int) : Option[Int] = {
    if (i == n) Some(n1 + n2)
    // else if (i == 1) loop(i + 1, 1, 0) // this line can replace 2 lines below, but is less clear
    else if (i == 1) loop(i + 1, 0, 1)
    else if (i == 2) loop(i + 1, 0, 1)
    else if (i < n) loop( i + 1 , n2, n1 + n2)
    else None
  }
  loop(1, 0, 0)
}

for(i <- 1 to 10) print(s"${fib(i).getOrElse(-1)}, ")


// worst-case space compl O(n), worst-case time compl O(n)
//class Tree(var x: Int, var l: Tree, var r: Tree)
sealed trait Tree[+A]
case object Leaf extends Tree[Nothing]
case class Branch[+A] (value: A, left: Tree[A], right: Tree[A]) extends Tree[A]

def amplitude(tree : Tree[Int]) : Int = {
    def dist(x : Int, y : Int) = math.abs(x - y)
    def maxIntervalBorders(p : (Int, Int), n : Int): (Int, Int) = {
      val _1 = dist(p._1, p._2)
      val _2 = dist(p._1, n)
      val _3 = dist(p._2, n)

      if(_1 >= _2 && _1 >= _3) p
      else if (_2 >= _3) (p._1, n)
      else (p._2, n)
    }

    def loop(t: Tree[Int], borders: (Int, Int)): Int = t match {
      case Leaf => dist(borders._1, borders._2)
      case Branch(v, l, r) => {
        val mdp = maxIntervalBorders(borders,v)
        math.max(loop(l, mdp), loop(r, mdp))
      }
    }

    tree match {
      case Leaf => 0
      case Branch(v, _, _) => loop(tree, (v,v))
    }
  }


val tree = Branch(5, Branch(8, Branch(12, Leaf, Leaf), Branch(2, Leaf, Leaf)),
  Branch(9, Branch(7, Branch(1, Leaf, Leaf), Leaf), Branch(4, Branch(3, Leaf, Leaf), Leaf)))

amplitude(tree)