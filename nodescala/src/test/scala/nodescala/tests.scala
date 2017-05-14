package nodescala

import scala.language.postfixOps
import scala.util.{Try, Success, Failure}
import scala.collection._
import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.async.Async.{async, await}
import org.scalatest._
import NodeScala._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class NodeScalaSuite extends FunSuite {

  test("A Future should always be completed") {
    val always = Future.always(517)

    assert(Await.result(always, 0 nanos) == 517)
  }
  test("A Future should never be completed") {
    val never = Future.never[Int]

    try {
      Await.result(never, 1 second)
      assert(false)
    } catch {
      case t: TimeoutException => // ok!
    }
  }
  test("Any should produce different results") {
    val any = Future.any(List(Future { 1 }, Future { 2 }, Future { throw new Exception }))

    try {
      val res = Await.result(any, 0 nanos)
      assert( res == 1 || res == 2)
    } catch {
      case t: TimeoutException => // ok!
    }
  }

  /*test("A Future should not complete after 1s when using a delay of 3s") {    
     try {
      val f1 = Future.delay(3 seconds)    
      Await.result(f1, 1 second)
      assert(false)
     } catch {
       case ex: TimeoutException => // OK!
     }
     
     try {
      val f2 = Future.delay(3 seconds)    
      Await.result(f2, 5 seconds)
      // OK!
     } catch {
       case ex: TimeoutException => assert(false)
     }
    }*/
  
  test("Test now") {
     assert(1 === Future.always(1).now)
     try { 
       Future.never.now;
       assert(false, "Should throw TimeoutException") } 
     catch {
       case t : TimeoutException => }
     try { 
       Future.delay(100 seconds).now; 
       println("fdsfdas")
       assert(false, "Should throw NoSuchElementException") } 
     catch { case t : NoSuchElementException => }
   }

  test("continueWith - on failure calls continuation") {
    val broken: Future[Int] = Future.failed(new IllegalArgumentException("ups"))
    var value = 1000
    val result: Future[Int] = broken.continueWith {
      f =>
        assert(f.isCompleted)
        assert(f.value.get.isFailure)
        value = 55
        value
    }
    assert(Await.result(result, 1 second) === 55)
  }

  test("continueWith - on success calls continuation") {
    val good: Future[Int] = Future.always[Int](1)
    var value = 1000
    val result: Future[Int] = good.continueWith {
      f =>
        assert(f.isCompleted)
        assert(f.value.get.isSuccess)
        value = 55
        value
    }
    assert(Await.result(result, 1 second) === 55)
  }

  test("CancellationTokenSource should allow stopping the computation also using Future.run() construct") {
    val p = Promise[String]()
    def fut(ct: CancellationToken): Future[Unit] = {
      Future {
        //async {
        while (ct.nonCancelled) {
          // do some work
        }
        p.success("done")
        //}
      }
    }
    val working = Future.run() { ct => fut(ct) }
    // take care the promise/future is not finalized before the unsubscribe
    // is called on the CancellationTokenSource()
    working.unsubscribe()
    assert(Await.result(p.future, 1 second) == "done")
  }

  
  
  class DummyExchange(val request: Request) extends Exchange {
    @volatile var response = ""
    val loaded = Promise[String]()
    def write(s: String) {
      response += s
    }
    def close() {
      loaded.success(response)
    }
  }

  class DummyListener(val port: Int, val relativePath: String) extends NodeScala.Listener {
    self =>

    @volatile private var started = false
    var handler: Exchange => Unit = null

    def createContext(h: Exchange => Unit) = this.synchronized {
      assert(started, "is server started?")
      handler = h
    }

    def removeContext() = this.synchronized {
      assert(started, "is server started?")
      handler = null
    }

    def start() = self.synchronized {
      started = true
      new Subscription {
        def unsubscribe() = self.synchronized {
          started = false
        }
      }
    }

    def emit(req: Request) = {
      val exchange = new DummyExchange(req)
      if (handler != null) handler(exchange)
      exchange
    }
  }

  class DummyServer(val port: Int) extends NodeScala {
    self =>
    val listeners = mutable.Map[String, DummyListener]()

    def createListener(relativePath: String) = {
      val l = new DummyListener(port, relativePath)
      listeners(relativePath) = l
      l
    }

    def emit(relativePath: String, req: Request) = this.synchronized {
      val l = listeners(relativePath)
      l.emit(req)
    }
  }
  test("Server should serve requests") {
    val dummy = new DummyServer(8191)
    val dummySubscription = dummy.start("/testDir") {
      request => for (kv <- request.iterator) yield (kv + "\n").toString
    }

    // wait until server is really installed
    Thread.sleep(500)

    def test(req: Request) {
      val webpage = dummy.emit("/testDir", req)
      val content = Await.result(webpage.loaded.future, 1 second)
      val expected = (for (kv <- req.iterator) yield (kv + "\n").toString).mkString
      assert(content == expected, s"'$content' vs. '$expected'")
    }

    test(immutable.Map("StrangeRequest" -> List("Does it work?")))
    test(immutable.Map("StrangeRequest" -> List("It works!")))
    test(immutable.Map("WorksForThree" -> List("Always works. Trust me.")))

    dummySubscription.unsubscribe()
  }

}




