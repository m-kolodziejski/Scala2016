package reductions

import java.util.concurrent._
import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._
import java.util.concurrent.ForkJoinPool.ForkJoinWorkerThreadFactory

@RunWith(classOf[JUnitRunner]) 
class LineOfSightSuite extends FunSuite {
  import LineOfSight._
  test("lineOfSight should correctly handle an array of size 4") {
    val output = new Array[Float](4)
    lineOfSight(Array[Float](0f, 1f, 8f, 9f), output)
    assert(output.toList == List(0f, 1f, 4f, 4f))
  }


  test("upsweepSequential should correctly handle the chunk 1 until 4 of an array of 4 elements") {
    val res = upsweepSequential(Array[Float](0f, 1f, 8f, 9f), 1, 4)
    assert(res == 4f)
  }

  test("upsweep should correctly handle the chunk 1 until 4 of an array of 4 elements") {
    val res = upsweep(Array[Float](0f, 1f, 8f, 9f, 12f), 1, 5, 1)
    assert(res == Node(Node(Leaf(1,2,1.0f),Leaf(2,3,4.0f)),Node(Leaf(3,4,3.0f), Leaf(4,5,3.0f))))
  }


  test("downsweepSequential should correctly handle a 4 element array when the starting angle is zero") {
    val output = new Array[Float](4)
    downsweepSequential(Array[Float](0f, 1f, 8f, 9f), output, 0f, 1, 4)
    assert(output.toList == List(0f, 1f, 4f, 4f))
  }

  test("downsweepSequential should correctly handle a 5 element array when the starting angle is zero") {
    val output = new Array[Float](5)
    downsweepSequential(Array[Float](0f, 7f, 7f, 33f, 48f), output, 0f, 1, 5)
    assert(output.toList == List(0f, 7f, 7f, 11f, 12f))
  }

  test("downsweep should correctly handle a 5 element array when the starting angle is zero") {
    val output = new Array[Float](5)
    downsweep(Array[Float](0f, 7f, 7f, 33f, 48f), output, 8f, Leaf(1,5,12f))
    assert(output.toList == List(0f, 7f, 7f, 11f, 12f))
  }
}

