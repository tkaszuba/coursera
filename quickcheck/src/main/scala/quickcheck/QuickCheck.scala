package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._ 

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { a: Int =>
    val h = insert(a, empty)
    isEmpty(deleteMin(h))
  }

  property("min2a") = forAll { a: Int =>
    val h = insert(a, empty)
    deleteMin(h) == empty
  }

  property("gen1") = forAll { (h: H) =>
	  val m = if (isEmpty(h)) 0 else findMin(h)
	  findMin(insert(m, h))==m
	}

   property("min3") = forAll { (a: Int, b:Int) =>
    (a < b) ==> {
    val h = insert(b,insert(a, empty))
    findMin(h) == a
    }
  }
   
  property("gen2") = forAll { (h1: H, h2: H) =>
      val m = meld(h1, h2)
      findMin(m) == findMin(h1) || findMin(m) == findMin(h2)
	}

  property("gen3") = forAll { (a: List[Int]) =>
  	val h = insert(a,empty)
    val x: Seq[A] = min(h,Seq())
    //println("orig: " + a +" new: "+ x)
    //x == x.sorted && 
    a.sorted == x
  }

  def insert(a: List[Int], h:H):H={
    if (a.isEmpty) h
    else
    	insert(a.tail,insert(a.head,h))
  }
  
  def length(h:H) : Int={
    if (isEmpty(h)) 0
    else
      1 + length(deleteMin(h)) 
  }

  def min(h:H,elem:Seq[A]) : Seq[A]={
    if (isEmpty(h)) elem
    else 
      elem ++ min(deleteMin(h),Seq(findMin(h))) 
  }
 
  
  lazy val genHeap: Gen[H] = for{
    k <- arbitrary[A]
    m <- oneOf(empty, genHeap).asInstanceOf[H]
  } yield insert(k,m)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
