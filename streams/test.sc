import streams._

object test {

  class Level1 extends GameDef with StringParserTerrain with Solver {
      /* terrain for level 1*/

    val level =
    """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin
      
      }
      
  val l = new Level1()                            //> l  : test.Level1 = test$Level1@3881bb1
  l.terrain(l.Pos(0,0))                           //> res0: Boolean = true
  
  Set((l.Block(l.Pos(1,2),l.Pos(1,3)), List(l.Right,l.Left,l.Up)),
    	(l.Block(l.Pos(2,1),l.Pos(3,1)), List(l.Down,l.Left,l.Up))).toStream.groupBy(e=>e._1)
                                                  //> res1: scala.collection.immutable.Map[test.l.Block,scala.collection.immutable
                                                  //| .Stream[(test.l.Block, List[Product with Serializable with test.l.Move])]] =
                                                  //|  Map(Block(Pos(1,2),Pos(1,3)) -> Stream((Block(Pos(1,2),Pos(1,3)),List(Right
                                                  //| , Left, Up)), ?), Block(Pos(2,1),Pos(3,1)) -> Stream((Block(Pos(2,1),Pos(3,1
                                                  //| )),List(Down, Left, Up)), ?))
    	
  /*l.newNeighborsOnly(
    	Set((l.Block(l.Pos(1,2),l.Pos(1,3)), List(l.Right,l.Left,l.Up)),
    	(l.Block(l.Pos(2,1),l.Pos(3,1)), List(l.Down,l.Left,l.Up))).toStream,
    	    Set(l.Block(l.Pos(1,2),l.Pos(1,3)), l.Block(l.Pos(1,1),l.Pos(1,1))))*/
 l.newNeighborsOnly(l.neighborsWithHistory(l.startBlock, List()),Set()).toSet
                                                  //> res2: scala.collection.immutable.Set[(test.l.Block, List[test.l.Move])] = Se
                                                  //| t((Block(Pos(1,2),Pos(1,3)),List(Right)), (Block(Pos(2,1),Pos(3,1)),List(Dow
                                                  //| n)))
    	    
}