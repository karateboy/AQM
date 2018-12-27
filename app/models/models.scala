package models
import play.api._
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

  implicit def getSqlDate(d: DateTime) = {
    new java.sql.Date(d.getMillis)
  }

  implicit def getSqlTime(d: DateTime) = {
    new java.sql.Time(d.getMillis)
  }

  def main(args: Array[String]) {
    val timestamp = DateTime.parse("2015-04-01")
    println(timestamp.toString())
  }

  def formatOptStr(strOpt: Option[String]) = {
    if (strOpt.isDefined)
      strOpt.get
    else
      "-"
  }

  def formatOptBool(boolOpt: Option[Boolean]) = {
    if (boolOpt.isDefined) {
      if (boolOpt.get)
        "是"
      else
        "否"
    } else
      "否"
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

object ExcelTool {
  import org.apache.poi.openxml4j.opc._
  import org.apache.poi.xssf.usermodel._
  import org.apache.poi.xssf.usermodel.XSSFSheet

  def getIntFromCell(cell: XSSFCell) = {
    try {
      cell.getNumericCellValue.toInt
    } catch {
      case ex: IllegalStateException =>
        cell.getStringCellValue.toInt
    }
  }

  def getStrFromCell(cell: XSSFCell) = {
    try {
      cell.getStringCellValue
    } catch {
      case ex: IllegalStateException =>
        cell.getNumericCellValue.toString
    }
  }

  def getOptionStrFromCell(cell: XSSFCell) = {
    try {
      Some(cell.getStringCellValue)
    } catch {
      case ex: Throwable =>
        None
    }
  }
  def getOptionDateFromCell(cell: XSSFCell) = {
    try {
      Some(cell.getDateCellValue)
    } catch {
      case ex: Throwable =>
        None
    }
  }

  import java.io._

  def importXLSX(filePath: String)(parser: (XSSFSheet) => Unit): Boolean = {
    val file = new File(filePath)
    importXLSX(file)(parser)
  }

  def importXLSX(file: File, delete: Boolean = false)(parser: (XSSFSheet) => Unit): Boolean = {
    Logger.info(s"Start import ${file.getAbsolutePath}...")
    //Open Excel
    try {
      val fs = new FileInputStream(file)
      val pkg = OPCPackage.open(fs)
      val wb = new XSSFWorkbook(pkg);

      parser(wb.getSheetAt(0))
      fs.close()
      if (delete)
        file.delete()
      Logger.info(s"Success import ${file.getAbsolutePath}")
    } catch {
      case ex: FileNotFoundException =>
        Logger.warn(s"Cannot open ${file.getAbsolutePath}")
        false
      case ex: Throwable =>
        Logger.error(s"Fail to import ${file.getAbsolutePath}", ex)
        false
    }
    true
  }
}

