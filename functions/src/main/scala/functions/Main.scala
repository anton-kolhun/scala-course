package functions

object Main {

  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = if (c <= 0 || c >= r || r <= 1) {
    1
  } else {
    pascal(c, r - 1) + pascal(c - 1, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {

    val invalidBalanceNumber = -100

    def balanceCounting(currentBalance: Int, remainingChars: List[Char]): Int = {
      if (currentBalance < 0) {
        invalidBalanceNumber
      } else remainingChars match {
        case Nil => currentBalance
        case ('(' :: tail) => balanceCounting(currentBalance + 1, tail)
        case (')' :: tail) => balanceCounting(currentBalance - 1, tail)
        case _ :: tail => balanceCounting(currentBalance, tail)
      }
    }

    val result = balanceCounting(0, chars)
    result == 0
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (money < 0 || coins.isEmpty) 0
    else countChange(money, coins.tail) + countChange(money - coins.head, coins)
  }

}
