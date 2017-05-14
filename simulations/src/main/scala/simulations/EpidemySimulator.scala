package simulations

import math.random

class EpidemySimulator extends Simulator {

  def randomBelow(i: Int) = (random * i).toInt

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8
    val prevelence: Double = 0.01
    val transmissibility: Double = 0.4
    val sicknessDelay: Int = 6
    val deathDelay: Int = 14
    val immunityDelay: Int = 16
    val healthyDelay: Int = 18
    val probabilityDeath: Double = 0.25
    val probabilityAir:Double = 0.01

    // to complete: additional parameters of simulation
  }

  import SimConfig._

  val persons: List[Person] = {for{
    num <- 1 to SimConfig.population
  } yield createPerson(num)}.toList // to complete: construct list of persons

  private def createPerson(id:Int):Person={
    val p = new Person(id)
    val ran = random*SimConfig.population
    if (id <= SimConfig.population*SimConfig.prevelence)
      regInfection(p)
    regMove(p)
    p
  }

  private def infect(p:Person)={
    if (p.isHealthy){
        val person = persons.find(f=>(f.row==p.row && f.col==p.col) || f.infected || f.sick || f.dead) 
		if (person.nonEmpty && (randomBelow(100) <= transmissibility*100))
		      regInfection(p)
        /*val personsMap = persons.groupBy(f=>(f.row,f.col))
		if (personsMap.contains(p.row,p.col)){
		   val inf = for{
			 person <- personsMap(p.row,p.col)
			 if (person.infected || person.sick || person.dead)
		    } yield true
		    
		    if (!inf.isEmpty && (randomBelow(100) <= transmissibility*100))
		      regInfection(p)
		 }*/
	 }
  }
  
  private def regMove(p:Person):Unit={
    val ran = randomBelow(5) + 1
    afterDelay(ran)({
      val rooms = sickRooms(p)
      if (randomBelow(100) <= probabilityAir*100)
        p.randomMove
      else if (p.move(rooms))
    	infect(p)
      regMove(p)
      })
  }
  
  private def borderedBySickRoom(p:Person):Boolean= !sickRooms(p).isEmpty

  private def sickRooms(p:Person):Set[(Int,Int)]={
    val neigh = p.neighbours
    val personsMap = persons.groupBy(f=>(f.row,f.col))

    val x = for{
      loc <- neigh
      if(personsMap.contains(loc))
      person <- personsMap(loc)
      if (person.sick || person.dead) 
    } yield loc
    x.toSet
  }
  
  private def regSick(p:Person):Unit={
    if (p.isInfected){
	    p.sick = true
	    afterDelay(deathDelay-sicknessDelay)(regDead(p))
    }
  }

  private def regDead(p:Person):Unit={
    if (p.isSick){
	    if(randomBelow(100) <= probabilityDeath*100) 
	      p.dead = true
	    else
	      afterDelay(immunityDelay-deathDelay)(regImmune(p))
    }
  }

  private def regImmune(p:Person):Unit={
    if (p.isSick){
	    p.immune = true
	    p.sick = false
	    afterDelay(healthyDelay-immunityDelay)(regHealthy(p))
    }
  }

  private def regHealthy(p:Person):Unit={
    if(p.isDead) println("Shouldn't be here")
    p.sick = false
    p.infected = false
    p.immune = false
  }

  private def regInfection(p:Person):Unit={
    if(!p.isInfected){
      p.infected = true
      afterDelay(sicknessDelay)(regSick(p))
    }
  }

  class Person (val id: Int) {
    var infected = false
    var sick = false
    var immune = false
    var dead = false

    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)
    
    def randomMove = {
    	row =  randomBelow(roomRows)
    	col =  randomBelow(roomRows)
    }
    def move(rooms:Set[(Int,Int)]):Boolean={
      if (!dead && rooms.size < 4){
          val available = Set(
             (if(row == roomRows-1) 0 else row + 1,col),
             (if(row == 0) roomRows-1 else row - 1,col),
             (row,if(col == roomColumns-1) 0 else col + 1),
             (row,if(col == 0) roomColumns-1 else col - 1))

           val diff = available.diff(rooms)
           if (diff.size == 0)
             throw new Error("Set difference can't be zero "+ available + rooms )

          if (diff.size == 1){
            row = diff.head._1
            col = diff.head._2
          }
          else{
            val sel = diff.toList(randomBelow(diff.size))
            //println("prev " + row + " " + col +" new "+sel._1+ " "+sel._2)
            row = sel._1
            col = sel._2
          }
          true
      }
      else
        false
    }
    
    def isHealthy:Boolean = !infected && !sick && !immune && !dead
    def isInfected:Boolean = infected && !sick && !immune && !dead
    def isSick:Boolean = infected && sick && !immune && !dead
    def isDead:Boolean = infected && sick && !immune && dead
    def isImmune:Boolean = infected && !sick && immune && !dead
    
    def neighbours:List[(Int,Int)]={
      val _rowR = if(row == roomRows-1) 0 else row + 1
      val _rowL = if(row == 0) roomRows-1 else row - 1
	  val _colD = if(col == roomColumns-1) 0 else col + 1 
	  val _colU = if(col == 0) roomColumns-1 else col - 1 
      List((_rowL,col),(_rowR,col),(row,_colD),(row,_colU))
    }
    
    //
    // to complete with simulation logic
    //
  }
}
