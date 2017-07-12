39/2


val list:List[Int] = List(1,3)

val medianScore: Int    = {
  def calcMean(mid: Int) = {
    list.sortBy(e => -e).toArray.apply(mid)
  }
  if((list.size)%2 == 1) {
    calcMean(list.size/2)
  }
  else {
    (calcMean(list.size/2)+calcMean((list.size - 1)/2))/2
  }

}


(list.sortBy(e => -e).toArray.apply(list.size/2)+list.sortBy(e => -e).toArray.apply((list.size - 1)/2))/2
list.size/2
(list.size - 1)/2