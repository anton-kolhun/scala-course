package lists

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class implements a ScalaTest test suite for the methods in object
 * `MaximumPathSum` that need to be implemented as part of this assignment. A test
 * suite is simply a collection of individual tests for some specific
 * component of a program.
 *
 * A test suite is created by defining a class which extends the type
 * `org.scalatest.FunSuite`. When running ScalaTest, it will automatically
 * find this class and execute all of its tests.
 *
 * Adding the `@RunWith` annotation enables the test suite to be executed
 * inside eclipse using the built-in JUnit test runner.
 *
 * You have two options for running this test suite:
 * 
 * - Start the sbt console and run the "test" command
 * - Right-click this file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class MaximumPathSumSuite extends FunSuite {
  
  import ProblemData._
  import ProblemSolver._
  
  test("one number problem") {
    val paths = findOptimalPaths(List(List(42)))
    val bestPath = chooseBestPath(paths)
    
    assert(bestPath.isDefined)
    assert(bestPath.get.sum=== 42)
    assert(numbersForPath(bestPath) === List(42))
  }
  
  test("sum of small problem") {
    val paths = findOptimalPaths(small)
    val bestPath = chooseBestPath(paths)

    assert(bestPath.isDefined)
    assert(bestPath.get.sum === 23)
  }

  test("path of small problem") {
    val paths = findOptimalPaths(small)
    val bestPath = chooseBestPath(paths)

    assert(bestPath.isDefined)
    assert(numbersForPath(bestPath) === List(3, 7, 4, 9))
  }

  test("sum of medium problem") {
    val paths = findOptimalPaths(medium)
    val bestPath = chooseBestPath(paths)
    
    assert(bestPath.isDefined)
    assert(bestPath.get.sum === 1074)
  }

  test("sum of large problem") {
    val paths = findOptimalPaths(large)
    val bestPath = chooseBestPath(paths)
    
    assert(bestPath.isDefined)
    assert(bestPath.get.sum === 7273)
  }
}
