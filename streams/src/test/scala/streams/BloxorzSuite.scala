package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Bloxorz._

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) => move match {
        case Left => block.left
        case Right => block.right
        case Up => block.up
        case Down => block.down
      }
    }
  }

  trait Level1 extends SolutionChecker {
      /* terrain for level 1*/

    val level =
    """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }

  test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0,0)), "0,0")
      assert(!terrain(Pos(0,4)), "0,4")
      assert(!terrain(Pos(4,11)), "4,11")
      assert(!terrain(Pos(-1,-1)), "-1,-1")
    }
  }

  test("findChar level 1") {
    new Level1 {
      assert(startPos === Pos(1,1))
      assert(goal === Pos(4,7))
    }
  }

  test("isLegal level 1") {
    new Level1 {
      assert(Block(Pos(1,1),Pos(1,1)).isLegal === true)
      assert(Block(Pos(0,0),Pos(0,0)).isLegal === true)
      assert(Block(Pos(0,2),Pos(0,4)).isLegal === false)
      assert(Block(Pos(2,0),Pos(4,0)).isLegal === false)
    }
  }

  test("isStanding level 1") {
    new Level1 {
      assert(Block(Pos(1,1),Pos(1,1)).isStanding === true)
      assert(Block(Pos(0,0),Pos(1,1)).isStanding === false)
      assert(Block(Pos(0,0),Pos(1,0)).isStanding === false)
    }
  }

  test("legal neighbours level 1") {
    new Level1 {
      assert(Block(Pos(0,0),Pos(0,0)).legalNeighbors === 
        List((Block(Pos(0,1),Pos(0,2)),Right), (Block(Pos(1,0),Pos(2,0)),Down)))
      assert(Block(Pos(1,1),Pos(1,1)).legalNeighbors === 
        List((Block(Pos(1,2),Pos(1,3)),Right), (Block(Pos(2,1),Pos(3,1)),Down)))
      assert(Block(Pos(1,2),Pos(1,2)).legalNeighbors ===
        List((Block(Pos(1,0),Pos(1,1)),Left), (Block(Pos(1,3),Pos(1,4)),Right), 
            (Block(Pos(2,2),Pos(3,2)),Down))) 
    }
  }

  test("neighborsWithHistory level 1") {
    new Level1 {
      assert(neighborsWithHistory(Block(Pos(0,0),Pos(0,0)), List()).toSet 
          === Set((Block(Pos(0,1),Pos(0,2)), List(Right)),(Block(Pos(1,0),Pos(2,0)), List(Down))))
      assert(neighborsWithHistory(Block(Pos(1,1),Pos(1,1)), List(Left,Up)).toSet 
          === Set((Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),(Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))))
    }
  }

  test("new neighborsOnly level 1") {
    new Level1 {
      assert(newNeighborsOnly(
    	Set((Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
    	    (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))).toStream, 
    	    Set(Block(Pos(1,2),Pos(1,3)), Block(Pos(1,1),Pos(1,1)))) ===
    	       Set((Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))).toStream)

    	       assert(newNeighborsOnly(
    	Set((Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
    	    (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))).toStream, 
    	    Set(Block(Pos(1,2),Pos(1,3)), Block(Pos(1,1),Pos(1,1)), 
    	        Block(Pos(2,1),Pos(3,1)))) ===
    	       Set().toStream)

    }
  }

  test("from level 1") {
    new Level1 {

     //println(mkString(pathsFromStart.take(2)))
     //println(mkString(pathsToGoal.take(5)))

      /*println(from(
    	Set((Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
    	    (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))).toStream, 
    	    Set(Block(Pos(1,2),Pos(1,3)), Block(Pos(1,1),Pos(1,1)))).apply(4))
*/
      /*assert(from(
    	Set((Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
    	    (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))).toStream, 
    	    Set(Block(Pos(1,2),Pos(1,3)), Block(Pos(1,1),Pos(1,1)))) ===
    	       Set((Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))).toStream)*/
    }
  }

  
  test("optimal solution for level 1") {
    new Level1 {
      assert(solve(solution) === Block(goal, goal))
    }
  }

  test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length == optsolution.length)
    }
  }
}
