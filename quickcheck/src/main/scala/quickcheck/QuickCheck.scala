package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    e <- arbitrary[A]
    h <- oneOf(const(empty), genHeap)
  } yield insert(e, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)


  override def findMin(h: H): A = h match {
    case t :: Nil => t.asInstanceOf[A]
    case t :: ts => {
      val min = findMin(ts.asInstanceOf[H])
      if (ord.lteq(t.asInstanceOf[A], min)) t.asInstanceOf[A] else min
    }

  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("heap with two elements") = forAll {
    (elem1: A, elem2: A) =>
      val h = insert(elem2, insert(elem1, empty))
      findMin(h) == ord.min(elem1, elem2)
  }

  property("delete min from 1 elem heap") = forAll {
    (elem: A) =>
      val h = insert(elem, empty)
      val hAfterDelete = deleteMin(h)
      isEmpty(hAfterDelete)
  }

  property("heap delete min create sequence") = forAll {
    (h: H) => nextMin(h)
  }

  def nextMin(h: H): Boolean = h match {
    case e :: Nil => true
    case e :: ee =>
      val hAfterDelete = deleteMin(h)
      ord.lteq(findMin(h), findMin(hAfterDelete)) && nextMin(hAfterDelete)
  }

  property("heap delete min create sequence 2") = forAll { h: H =>
    val hSub = deleteMinSub(h, Nil)
    hSub == hSub.sorted
  }

  def deleteMinSub(ts: H, as: List[Int]): List[Int] = {
    if (isEmpty(ts)) as
    else findMin(ts) :: deleteMinSub(deleteMin(ts), as)
  }

  property("minimum for melding") = forAll {
    (h1 : H, h2 : H) =>
      ord.compare(findMin(meld(h1, h2)), findMin(h1)) == 0 || ord.compare(findMin(meld(h1, h2)), findMin(h2)) == 0
  }

  property("adding less than min gives new min") = forAll {
    (elem : A, h : H) =>
      if(ord.lteq(elem, findMin(h))) {
        ord.compare(findMin(insert(elem, h)), elem) == 0
      }
      else {
        ord.compare(findMin(h), findMin(insert(elem,h))) == 0
      }
  }

  property("adding less than min and remove does not change the old min") = forAll {
    (elem : A, h : H) =>
      val min = findMin(h)
      if(ord.lteq(elem, findMin(h))) {
        ord.compare(findMin(deleteMin(insert(elem, h))), min) == 0
      }
      else {
        ord.compare(findMin(insert(elem, h)), min) == 0
      }
  }

  property("delete minimum and add it back") = forAll {
    (h : H) => {
      val min = findMin(h)
      ord.compare(min, findMin(insert(min, deleteMin(h)))) == 0
    }
  }

  property("heap with two elements") = forAll {
    (h : H, elem1: A, elem2: A) =>
      if(ord.lteq(elem1, findMin(h)) || ord.lteq(elem2, findMin(h))) {
        if (ord.lteq(elem1, elem2)) {
          findMin(insert(elem1, insert(elem2,h))) == elem1
        }
        else {
          findMin(insert(elem2, insert(elem1,h))) == elem1
        }
      }
        true
  }

  property("add min twice") = forAll {
    (h: H, elem : A) =>
      if(ord.lteq(elem, findMin(h))) {
        findMin(deleteMin(insert(elem, insert(elem, h)))) == elem
      }
      true
  }

 property("identity of melds after remove insert") = forAll {
   (h1 : H, h2 : H) =>
     val meld1 = meld(h1, h2)
     val meld2 = meld(insert(findMin(h2), h1), deleteMin(h2))
     deleteMinSub(meld1, Nil) == deleteMinSub(meld2, Nil)
 }
}
