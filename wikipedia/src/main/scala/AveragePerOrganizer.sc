import org.apache.spark.SparkContext

class Event(val organizer: String, name: String,val budget: Int) {

  val events: List[Event] = List()

  val sc: SparkContext = new SparkContext()
  val rdd = sc.parallelize(events.map(e => (e.organizer, e.budget)), 1)
  rdd.mapValues(b => (b, 1)).reduceByKey((v1, v2) => (v1._1+v2._1, v1._2+v2._2))

}

