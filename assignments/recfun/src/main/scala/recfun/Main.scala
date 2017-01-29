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
      if (c == 0 || c == r) 1
      else pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def loop(acc: Int, chars: List[Char]): Boolean = {
        if (acc < 0) false
        else chars match {
          case Nil => acc == 0
          case x :: tail => {
            x match {
              case '(' => loop(acc + 1, tail)
              case ')' => loop(acc - 1, tail)
              case _ => loop(acc, tail)
            }
          }
        }
      }
      loop(0, chars)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (money == 0) 1
      else if (money < 0) 0
      else coins match {
        case Nil => 0
        case x :: tail => countChange(money - x, coins) + countChange(money, tail)
      }
    }
  }
