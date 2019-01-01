package models
import play.api._
import akka.actor._
import com.github.nscala_time.time.Imports._
import play.api.Play.current
import akka.actor._
import scala.concurrent.ExecutionContext.Implicits.global
import play.api.libs.concurrent.Akka
import play.api.libs.json._

object DbUpdater {
  case object Check

  var worker: ActorRef = _

  def start = {
    worker = Akka.system.actorOf(Props[DbUpdater], name = "DbUpdater")
    worker ! Check
  }
}

class DbUpdater extends Actor {
  import DbUpdater._
  import ExcelTool._
  import java.io.File
  def receive = handler(None)

  def listAllFiles = {
    //import java.io.FileFilter
    val path = new java.io.File(current.path.getAbsolutePath + "/updateDB/")
    if (path.exists() && path.isDirectory()) {
      val allFiles = new java.io.File(current.path.getAbsolutePath + "/updateDB/").listFiles().toList
      allFiles.filter(p => p != null && p.getName.endsWith(".xlsx"))
    } else {
      List.empty[File]
    }
  }

  def handler(checkPointOpt: Option[DateTime]): Receive = {
    case Check =>
      for (file <- listAllFiles) {
        try {
          if (file.getName.contains("hour"))
            ExcelTool.importXLSX(file, true)(hourUpdateParser)
          else
            ExcelTool.importXLSX(file, true)(minUpdateParser)
        } catch {
          case ex: Throwable =>
            Logger.warn(s"Cannot handle ${file.getAbsolutePath}", ex)
        }
      }
  }

  import org.apache.poi.xssf.usermodel.XSSFSheet
  import scalikejdbc._
  def hourUpdateParser(sheet: XSSFSheet) = {
    def upsertHourRecord(mStr: String, mtStr: String, value: Double, status: String, year: Int, month: Int, day: Int, timeStr: String) = {
      val tab = Record.getTabName(TableType.Hour, year + 1911)
      val mtValueName = SQLSyntax.createUnsafely(mtStr.replaceFirst("2", "5") + "V")
      val mtStatusName = SQLSyntax.createUnsafely(mtStr.replaceFirst("2", "5") + "S")
      val dt = DateTime.parse(s"${year + 1911}/${month}/${day} ${timeStr}", DateTimeFormat.forPattern("YYYY/MM/dd HHmmss"))
      DB localTx {
        implicit session =>
          sql"""
            IF NOT EXISTS (SELECT * FROM $tab WHERE DP_NO=$mStr and M_DateTime = ${dt.toDate()})
              INSERT INTO $tab(DP_NO, M_DateTime, $mtValueName, $mtStatusName)
              VALUES($mStr, ${dt.toDate()}, $value, $status)
            ELSE
              UPDATE $tab
                SET  $mtValueName=$value, $mtStatusName=$status
                WHERE DP_NO=$mStr and M_DateTime = ${dt.toDate()}
            """.update.apply
      }
    }

    var rowN = 1
    var finish = false
    do {
      var row = sheet.getRow(rowN)
      if (row == null)
        finish = true
      else {
        try {
          import com.github.nscala_time.time.Imports._
          val mStr = row.getCell(0).getStringCellValue
          val mtStr = row.getCell(2).getStringCellValue
          val year = row.getCell(4).getStringCellValue.toInt
          val month = row.getCell(5).getStringCellValue.toInt
          val day = row.getCell(6).getStringCellValue.toInt
          val timeStr = row.getCell(7).getStringCellValue
          val value = row.getCell(8).getStringCellValue.toDouble
          val status = row.getCell(9).getStringCellValue
          upsertHourRecord(mStr, mtStr, value, status, year, month, day, timeStr)
        } catch {
          case ex: java.lang.NullPointerException =>
          // last row Ignore it...

          case ex: Throwable =>
            Logger.error(s"failed to convert row=$rowN...", ex)
        }
      }
      rowN += 1
    } while (!finish)
  }

  def minUpdateParser(sheet: XSSFSheet) = {
    def upsertMinRecord(mStr: String, mtStr: String, value: Double, status: String, dt: DateTime) = {
      val tab = Record.getTabName(TableType.Min, dt.getYear)
      val mtValueName = SQLSyntax.createUnsafely(mtStr + "V")
      val mtStatusName = SQLSyntax.createUnsafely(mtStr + "S")
      DB localTx {
        implicit session =>
          sql"""
            IF NOT EXISTS (SELECT * FROM $tab WHERE DP_NO=$mStr and M_DateTime = ${dt.toDate})
              INSERT INTO $tab(DP_NO, M_DateTime, $mtValueName, $mtStatusName)
              VALUES($mStr, ${dt}, $value, $status)
            ELSE
              UPDATE $tab
                SET  $mtValueName=$value, $mtStatusName=$status
                WHERE DP_NO=$mStr and M_DateTime = ${dt.toDate}
            """.update.apply
      }
    }

    var rowN = 1
    var finish = false
    do {
      var row = sheet.getRow(rowN)
      if (row == null)
        finish = true
      else {
        try {
          import com.github.nscala_time.time.Imports._
          val mStr = row.getCell(0).getStringCellValue
          val dt = DateTime.parse(row.getCell(1).getStringCellValue, DateTimeFormat.forPattern("YYYY/MM/dd HH:mm"))
          val mtStr = row.getCell(2).getStringCellValue
          val value = row.getCell(3).getNumericCellValue
          val status = row.getCell(4).getStringCellValue
          upsertMinRecord(mStr, mtStr, value, status, dt)
        } catch {
          case ex: java.lang.NullPointerException =>
          // last row Ignore it...

          case ex: Throwable =>
            Logger.error(s"failed to convert row=$rowN...", ex)
        }
      }
      rowN += 1
    } while (!finish)
  }

}