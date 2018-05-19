// worst-case space compl O(1), worst-case time compl O(n)
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


// worst-case space compl O(n), worst-case time compl O(n)
//class Tree(var x: Int, var l: Tree, var r: Tree)
sealed trait Tree[+A]
case object Leaf extends Tree[Nothing]
case class Branch[A] (value: A, left: Tree[A], right: Tree[A]) extends Tree[A]

def dist(x : Int, y : Int) = math.abs(x - y)
// valid, aka find intervals borders
def maxDiffPair(p : (Int, Int), n : Int): (Int, Int) = {
    val _1 = dist(p._1, p._2)
    val _2 = dist(p._1, n)
    val _3 = dist(p._2, n)

    if(_1 >= _2 && _1 >= _3) p
    else if (_2 >= _3) (p._1, n)
    else (p._2, n)
}

def amplitude(tree : Tree[Int]) : Int = {
    def loop(t: Tree[Int], pair: (Int, Int)): Int = t match {
      case Leaf => dist(pair._1, pair._2)
      case Branch(v, l, r) => {
        val mdp = maxDiffPair(pair,v)

        val _l = loop(l, mdp)
        val _r = loop(r, mdp)

        if(_l >= _r) _l else _r
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