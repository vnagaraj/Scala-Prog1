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
  // test("string take") {
  //   val message = "hello, world"
  //   assert(message.take(5) == "hello")
  // }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  // test("adding ints") {
  //   assert(1 + 2 === 3)
  // }


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
  test("singletonSet(1) contains 1") {

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
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersection contains all elements of each set") {
    new TestSets {
      val s = intersect(s1, s2)
      assert(!contains(s, 1), "Intersect 1 false")
      assert(!contains(s, 2), "Intersect 2 false")
      assert(!contains(s, 3), "Intersect 3 false")
      val unionS = union(s1, s2)
      val intersectS = intersect(unionS, s1)
      assert(contains(intersectS, 1), "Intersect 1 true")
      assert(!contains(intersectS, 2), "Intersect 2 false")
      assert(!contains(intersectS, 3), "Intersect 3 false")
    }
  }

  test("difference between elements") {
    new TestSets {
      val s = diff(s1, s2)
      assert(contains(s, 1), "Difference 1 true")
      assert(!contains(s, 2), "Difference 2 false")
      assert(!contains(s, 3), "Difference 3 false")
      val unionS = union(s1, s2)
      val diffS = diff(unionS, s1)
      assert(!contains(diffS, 1), "Difference 1 false")
      assert(contains(diffS, 2), "Difference 2 true")
      assert(!contains(diffS, 3), "Difference 3 false")
    }
  }

  test("filter tests") {
    new TestSets {
      val unionS = union(s1, s2)
      val filterS = filter(unionS, s1)
      assert(contains(filterS, 1), "Filter 1 true")
      assert(!contains(filterS, 2), "Filter 2 false")
      assert(!contains(filterS, 3), "Filter 3 false")
    }
  }

  test("for all tests") {
    new TestSets {
      val s4 = union(s1, s2)
      val unionS = union(s4, s3) // unionS comprises 1, 2, 3
      val p : Int=>Boolean = x => x < 4

      val filterS = filter(unionS, s1)
      assert(forall(union(s4, s3), x => x < 4), "Filter 1 true")
    }
  }

  test("map tests") {
    new TestSets {
      val s4 = singletonSet(4)
      val s5 = singletonSet(5)
      val s7 = singletonSet(7)
      val s1000 = singletonSet(1000)
      val unionS1S3 = union(s1, s3)
      val unionS1S3S4 = union(unionS1S3, s4)
      val unionS1S3S4S5 = union(unionS1S3S4, s5)
      val unionS1S3S4S5S7 = union(unionS1S3S4S5, s7)
      val unionS1S3S4S5S7S1000 = union(unionS1S3S4S5S7, s1000)
      def f: Int=>Int = x => x-1
      val m = map(unionS1S3S4S5S7S1000, f)
      assert(contains(unionS1S3S4S5S7S1000, 1), "Set true")
      assert(contains(m, 0), "Map true")
      assert(contains(unionS1S3S4S5S7S1000, 3), "Set true")
      assert(contains(m, 2), "Set true")
      assert(contains(unionS1S3S4S5S7S1000, 4), "Set true")
      assert(contains(m, 999), "Set true")
    }
  }


}
