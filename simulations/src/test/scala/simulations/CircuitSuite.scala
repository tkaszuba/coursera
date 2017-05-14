package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CircuitSuite extends CircuitSimulator with FunSuite {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5
  
  test("andGate example") {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    
    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    run
    
    assert(out.getSignal === false, "and 2")

    in2.setSignal(true)
    run
    
    assert(out.getSignal === true, "and 3")
  }

  test("orGate example") {
    val in1, in2, out = new Wire
    orGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    
    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    run
    
    assert(out.getSignal === true, "and 2")

    in1.setSignal(false)
    in2.setSignal(true)
    run
    
    assert(out.getSignal === true, "and 3")

    in1.setSignal(true)
    run
    
    assert(out.getSignal === true, "and 4")
  }

  test("orGate2 example") {
    val in1, in2, out = new Wire
    orGate2(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    
    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    run
    
    assert(out.getSignal === true, "and 2")

    in1.setSignal(false)
    in2.setSignal(true)
    run
    
    assert(out.getSignal === true, "and 3")

    in1.setSignal(true)
    run
    
    assert(out.getSignal === true, "and 4")
  }
  
  test("demux example 1 to 1") {
    val in = new Wire
    val c:List[Wire] = List()
    val out:List[Wire] = List(new Wire)
    
    demux(in, c, out)
    in.setSignal(false)
    run
    
    assert(out(0).getSignal === false, "and 1")
  }

  test("demux example 1 to 2") {
    val in, c0, out0, out1 = new Wire
    demux(in, List(c0), List(out1,out0))
    
    in.setSignal(false)
    c0.setSignal(false)
    run
    
    assert(out0.getSignal === false, "and 1")
    assert(out1.getSignal === false, "and 2")

    c0.setSignal(true)
    run
    
    assert(out0.getSignal === false, "and 3")
    assert(out1.getSignal === false, "and 4")

    in.setSignal(true)
    run
    
    assert(out0.getSignal === false, "and 5")
    assert(out1.getSignal === true, "and 6")
    
    c0.setSignal(false)
    run
    
    assert(out0.getSignal === true, "and 7")
    assert(out1.getSignal === false, "and 8")
 
  }

  test("demux example 1 to 4") {
	val in, c0, c1, out0, out1, out2, out3 = new Wire
    demux(in, List(c1,c0), List(out3,out2,out1,out0))
 
    println("1:4")
    
    in.setSignal(false)
    c0.setSignal(false)
    c1.setSignal(false)
    run
    
    assert(out0.getSignal === false, "and 1")
    assert(out1.getSignal === false, "and 2")
    assert(out2.getSignal === false, "and 3")
    assert(out3.getSignal === false, "and 4")

    in.setSignal(true)
    run
    
    assert(out0.getSignal === true, "and 5")
    assert(out1.getSignal === false, "and 6")    
    assert(out2.getSignal === false, "and 7")
    assert(out3.getSignal === false, "and 8")
    

    in.setSignal(false)
    c0.setSignal(true)
    run
    
    assert(out0.getSignal === false, "and 9")
    assert(out1.getSignal === false, "and 10")
    assert(out2.getSignal === false, "and 11")
    assert(out3.getSignal === false, "and 12")
    
    println("here")
    in.setSignal(true)
    run
    
    assert(out0.getSignal === false, "and 13")
    assert(out1.getSignal === true, "and 14")
    assert(out2.getSignal === false, "and 15")
    assert(out3.getSignal === false, "and 16")

    in.setSignal(false)
    c0.setSignal(false)
    c1.setSignal(true)
    run
    
    assert(out0.getSignal === false, "and 17")
    assert(out1.getSignal === false, "and 18")
    assert(out2.getSignal === false, "and 19")
    assert(out3.getSignal === false, "and 20")

    in.setSignal(true)
    run
    
    assert(out0.getSignal === false, "and 21")
    assert(out1.getSignal === false, "and 22")
    assert(out2.getSignal === true, "and 23")
    assert(out3.getSignal === false, "and 24")

    in.setSignal(false)
    c1.setSignal(true)
    c0.setSignal(true)
    run
    
    assert(out0.getSignal === false, "and 25")
    assert(out1.getSignal === false, "and 26")
    assert(out2.getSignal === false, "and 27")
    assert(out3.getSignal === false, "and 28")

    in.setSignal(true)
    run
    
    assert(out0.getSignal === false, "and 29")
    assert(out1.getSignal === false, "and 30")
    assert(out2.getSignal === false, "and 31")
    assert(out3.getSignal === true, "and 32")


  }
  implicit def boolean2Int(b: Boolean) = if (b) 1 else 0
  implicit def int2boolean(bit: Int) = if (bit > 0) true else false

  def setSignals(wires: List[Wire], signals: Int*) {
    (wires zip signals) foreach { case (wire, signal) => wire setSignal signal }
  }
  
  test("demux 1-3-8") {
    val in = new Wire
    val control = List.fill(3)(new Wire)
    val out = List.fill(8)(new Wire)
    demux(in, control, out)
    in.setSignal(1)

    setSignals(control, 0, 0, 0); run
    assert(out.map(_.getSignal.toInt) === List(0, 0, 0, 0, 0, 0, 0, 1))

    setSignals(control, 0, 0, 1); run
    assert(out.map(_.getSignal.toInt) === List(0, 0, 0, 0, 0, 0, 1, 0))

    setSignals(control, 0, 1, 0); run
    assert(out.map(_.getSignal.toInt) === List(0, 0, 0, 0, 0, 1, 0, 0))

    setSignals(control, 0, 1, 1); run
    assert(out.map(_.getSignal.toInt) === List(0, 0, 0, 0, 1, 0, 0, 0))

    setSignals(control, 1, 0, 0); run
    assert(out.map(_.getSignal.toInt) === List(0, 0, 0, 1, 0, 0, 0, 0))

    setSignals(control, 1, 0, 1); run
    assert(out.map(_.getSignal.toInt) === List(0, 0, 1, 0, 0, 0, 0, 0))

    setSignals(control, 1, 1, 0); run
    assert(out.map(_.getSignal.toInt) === List(0, 1, 0, 0, 0, 0, 0, 0))

    setSignals(control, 1, 1, 1); run
    assert(out.map(_.getSignal.toInt) === List(1, 0, 0, 0, 0, 0, 0, 0))
    //    println(out.map(_.getSignal.toInt))
  }

  //
  // to complete with tests for orGate, demux, ...
  //

}
