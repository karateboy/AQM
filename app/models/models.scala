package models
import com.github.nscala_time.time.Imports._

/**
 * @author user
 */

object ModelHelper {
  implicit def getSqlTimestamp(t:DateTime) ={
    new java.sql.Timestamp(t.getMillis)
  }

    def main(args: Array[String]) {
      val timestamp = DateTime.parse("2015-04-01")
      println(timestamp.toString())
    }
}