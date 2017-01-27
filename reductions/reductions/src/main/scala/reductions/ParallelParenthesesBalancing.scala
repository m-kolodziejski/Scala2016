package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    def balanceWithCount(chars: Array[Char], counter: Int, idx: Int): Boolean = {
      if (idx >= chars.size) {
        counter == 0
      } else if(counter < 0) {
        false
      } else if(chars(idx) == '(') {
        balanceWithCount(chars, counter+1, idx+1)
      } else if(chars(idx) == ')') {
        balanceWithCount(chars, counter-1, idx+1)
      } else {
        balanceWithCount(chars, counter, idx+1)
      }
    }
    balanceWithCount(chars, 0, 0)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int) : (Int, Int) = {
      if(idx < until) {
        var nextArg2 = arg2

        if (chars(idx) == '(') {
          traverse(idx+1, until, arg1+1, nextArg2)
        } else if (chars(idx) == ')') {
          if(arg1 - 1< 0) {
            nextArg2 = arg1 - 1
          }
          traverse(idx+1, until, arg1-1, nextArg2)
        }
        else {
          traverse(idx+1, until, arg1, nextArg2)
        }
      }
      else {
        (arg1, arg2)
      }
    }

    def reduce(from: Int, until: Int) : (Int, Int) = {
      if(threshold >= until - from) {
        traverse(from, until, 0, 0)
      }
      else {
        val (l: (Int,Int), r: (Int, Int)) = parallel(reduce(from, (from+until)/2), reduce((from+until)/2 , until))
        if(l._2 < 0) {
          (l._1+r._1, l._2)
        } else if(r._2 < 0 && ( l._1+r._1 ) <0) {
          (l._1+r._1, r._2)
        }
        else {
          (l._1+r._1, 0)
        }
      }
    }

    reduce(0, chars.length) == (0,0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
