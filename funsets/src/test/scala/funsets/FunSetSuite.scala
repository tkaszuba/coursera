package funsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {


  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  test("string take") {
    val message = "hello, world"
    assert(message.take(5) == "hello")
  }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  test("adding ints") {
    assert(1 + 2 === 3)
  }

  
  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }
  
  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   * 
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   * 
   *   val s1 = singletonSet(1)
   * 
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   * 
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   * 
   */

  trait TestSets {
    val sNeg = singletonSet(-3)
    val s0 = singletonSet(0)
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   * 
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test ("singletonSet(1) contains 1") {
    
    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3". 
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
      assert(contains(s2, 2), "Singleton")
      assert(contains(s3, 3), "Singleton")
      assert(contains(s0, 0), "Singleton") 
      assert(contains(sNeg, -3), "Singleton") 
      assert(!contains(s1, 2), "Should return false")
      assert(!contains(s2, 1), "Should return false")
      assert(!contains(s3, 1), "Should return false")
      assert(!contains(s0, 1), "Should return false")
      assert(!contains(sNeg, 1), "Should return false")      
      
    }
  }

  test ("union contains all elements") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
      
      val y = union(x => x==1 || x==2 || x==3,x => x==2 || x==3 || x==4)
      assert(contains(y, 1), "Union 1")
      assert(contains(y, 3), "Union 3")
      assert(contains(y, 4), "Union 3")
      
      assert(contains(union(x=>false,x=> x==1),1), "empty set")

    } 
  }
  
  test ("interect of all elements") {
    new TestSets {
      val s = intersect(s1, s1)
      assert(contains(s, 1), "intersect 1")
      assert(!contains(s, 2), "intersect 2")
      
      val y = intersect(x => x==1 || x==2 || x==3,x => x==2 || x==3 || x==4)
      assert(!contains(y, 1), "intersect 1a")
      assert(contains(y, 3), "intersect 3")
      assert(!contains(y, 4), "intersect 4")

    }
  }

  test ("diff of diff elements") {
    new TestSets {
      val s = diff(s1, s2)
      assert(contains(s, 1), "diff 1")
      assert(!contains(s, 2), "diff 2")
      assert(!contains(s, 3), "diff 3")
      
      val y = diff(x => x==1 || x==2 || x==3,x => x==2 || x==3 || x==4)
      assert(contains(y, 1), "intersect 1a")
      assert(!contains(y, 3), "intersect 3")
      assert(!contains(y, 4), "intersect 4")

    }
  }
  
  test("filter of {1,3,4,5,7,1000} for _ < 5") {

    new TestSets {
      val s = filter(x => x == 1 || x == 3 || x == 4 || x == 5 || x == 7 || x == 1000, x => x < 5)
  
      assert(contains(s, 1), "map 1")
      assert(contains(s, 3), "map 3")
      assert(contains(s, 4), "map 4")
      assert(!contains(s, 5), "map 5")
      assert(!contains(s, 7), "map 7")
      assert(!contains(s, 1000), "map 1000")
    }

  }

    test("forall of {2,4,6,8} for _ < x % 2") {
    new TestSets {
      val s = forall(x => x == 2 || x == 4 || x == 6 || x == 8 , x => x % 2 == 0)
      assert(s, "should be true")
      
      val y = forall(x => x == 2 || x == 4 || x == 5 , x => x % 2 == 0)
      assert(!y, "should be false")

      val z = forall(x => false , x => x % 2 == 0)
      assert(z, "should be true")
    }
    }
    
    test("exists of {2,4,6,8} for _ < x % 2") {
    new TestSets {
      val s = exists(x => x == 2 || x == 4 || x == 6 || x == 8 , x => x % 2 == 0)
      assert(s, "exists 1 should be true")
      
      val y = exists(x => x == 2 || x == 4 || x == 5 , x => x % 2 == 0)
      assert(y, "exists 2 should be true")

      val z = exists(x => false , x => x % 2 == 0)
      assert(z, "exists should be true")
    }
     
  }

}
