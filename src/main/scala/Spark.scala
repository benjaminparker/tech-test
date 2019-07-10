import org.apache.spark.sql.SparkSession

object Spark {

  val spark = SparkSession.builder.appName("Wejo Tech Test").getOrCreate()



}
