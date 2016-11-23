package recfun

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
    def pascal(c: Int, r: Int): Int = {
      if (c < 0 || r < 0) throw new RuntimeException("column/row cannot be negative")
      if (c > r) throw new RuntimeException("column should be less than row")
      if (c == r || c == 0) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
       def check(acc:Int, chars: List[Char]):Int = {
         if (chars.isEmpty) acc
         else if (chars.head == '(') check(acc+1, chars.tail)
         else if (chars.head == ')' && acc == 0) 1
         else if (chars.head == ')') check (acc-1, chars.tail)
         else check (acc, chars.tail)
       }
      check(0, chars) == 0
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (money == 0) 1
      else if (money < 0) 0
      else if (coins.isEmpty) 0
      else countChange(money, coins.tail) + countChange(money - coins.head, coins)
    }
  }
