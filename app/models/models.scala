package models
import com.github.nscala_time.time.Imports._
import scala.language.implicitConversions
import play.api.libs.json._
import play.api.libs.functional.syntax._
import scala.util.parsing.json.JSONFormat

/**
 * @author user
 */

object ModelHelper {
  implicit def getSqlTimestamp(t: DateTime) = {
    new java.sql.Timestamp(t.getMillis)
  }

  implicit def getDateTime(st: java.sql.Timestamp) = {
    new DateTime(st)
  }

  implicit def getDateTime(d: java.sql.Date) = {
    new DateTime(d)
  }
  
  implicit def getDateTime(t: java.sql.Time) = {
    new DateTime(t)
  }
  
  implicit def getSqlDate(d:DateTime) = {
    new java.sql.Date(d.getMillis)  
  }
  
  implicit def getSqlTime(d:DateTime) = {
    new java.sql.Time(d.getMillis)
  }
  
  def main(args: Array[String]) {
    val timestamp = DateTime.parse("2015-04-01")
    println(timestamp.toString())
  }
}

object EnumUtils {
  def enumReads[E <: Enumeration](enum: E): Reads[E#Value] = new Reads[E#Value] {
    def reads(json: JsValue): JsResult[E#Value] = json match {
      case JsString(s) => {
        try {
          JsSuccess(enum.withName(s))
        } catch {
          case _: NoSuchElementException => JsError(s"Enumeration expected of type: '${enum.getClass}', but it does not appear to contain the value: '$s'")
        }
      }
      case _ => JsError("String value expected")
    }
  }

  implicit def enumWrites[E <: Enumeration]: Writes[E#Value] = new Writes[E#Value] {
    def writes(v: E#Value): JsValue = JsString(v.toString)
  }

  implicit def enumFormat[E <: Enumeration](enum: E): Format[E#Value] = {
    Format(enumReads(enum), enumWrites)
  }
}
