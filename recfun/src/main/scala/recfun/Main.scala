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
    def pascal(c: Int, r: Int): Int = 
        if (r== 0 || c==0 || c==r) 1 else  pascal(c,r-1) + pascal(c-1,r-1)
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
            def helper(x:List[Char],state:Int):Int={
      if (x.isEmpty || state == -1 ) 
        state
      else if (x.head == '(')
         helper(x.tail, state+1)
      else if (x.head == ')')
        helper(x.tail, state-1)
      else
        helper(x.tail,state)    
      }
    
    helper(chars,0) == 0
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {    
    
    def cc(amount: Int, kindofcoins: List[Int]):Int ={
      if (amount == 0) 1
      else if ((amount < 0) || (kindofcoins.isEmpty)) 
        0
      else 
        cc(amount, kindofcoins.tail) + cc(amount - kindofcoins.head, kindofcoins)
    } 
    cc(money,coins)
    
  }
}
