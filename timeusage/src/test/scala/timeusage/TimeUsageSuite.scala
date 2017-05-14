package timeusage

import org.apache.spark.sql.{ColumnName, DataFrame, Row}
import org.apache.spark.sql.types.{
  DoubleType,
  StringType,
  StructField,
  StructType
}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfterAll, FunSuite}

import scala.util.Random

@RunWith(classOf[JUnitRunner])
class TimeUsageSuite extends FunSuite with BeforeAndAfterAll {
  
  import org.apache.spark.sql.SparkSession
  import org.apache.spark.sql.functions._

  test("testColumns") {
    val list = Set("t1112", "t180101", "t0102", "t0301", "t010101", "t180301").toList
    val expected = list.map { x => col(x) }

    assert(expected.toSet === TimeUsage.classifiedColumns(list)._1.toSet)
    
  }

}
