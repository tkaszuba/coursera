object test {
	import simulations._
	import gui._
	
	  def randomBelow(i: Int) = (math.random * i).toInt
                                                  //> randomBelow: (i: Int)Int
	
	                              randomBelow(3)
                                                  //> res0: Int = 2
}