/**
  * Exercise 1
  */
def pascal(c: Int, r: Int): Int =
  if (c == 0 || c == r)
    1
  else pascal (c - 1, r - 1) + pascal (c, r - 1)

for (row <- 0 to 10) {
    for (col <- 0 to row)
        print(pascal(col, row) + " ")
    println()
}


/**
  * Exercise 2
  */
def balance(chars: List[Char]): Boolean =
{
    def loop(chars : List[Char], balance: Int): Boolean = {
        if(chars.isEmpty) balance == 0
        else
        {
            if(balance < 0) false
            else if (chars.head == '(')
                loop(chars.tail, balance + 1)
            else if (chars.head == ')')
                loop(chars.tail, balance - 1)
            else
                loop(chars.tail, balance)
        }
    }
    loop(chars, 0)
}

/**
  * Exercise 3
  */
def countChange(money: Int, coins: List[Int]): Int = {
    val sortedCoins = coins.sorted
    if (sortedCoins.isEmpty || money - sortedCoins.head < 0)
        0
    else if (money - sortedCoins.head == 0)
        1
    else countChange(money - sortedCoins.head, sortedCoins) + countChange(money, sortedCoins.tail)
}