package controllers
import play.api._
import play.api.Play.current
import org.apache.poi.openxml4j.opc._
import org.apache.poi.xssf.usermodel._
import models.DailyReport
import controllers.Report._
import models.Monitor
import models.MonitorStatus
import models.ModelHelper._
import com.github.nscala_time.time.Imports._
import java.io._
import java.nio.file.Files
import java.nio.file._

/**
 * @author user
 */
object ExcelUtility {
  val docRoot = "/report_template/"

  private def prepareTemplate(templateFile: String) = {
    val templatePath = Paths.get(current.path.getAbsolutePath + docRoot + templateFile)
    val reportFilePath = Files.createTempFile("temp", ".xlsx");

    Files.copy(templatePath, reportFilePath, StandardCopyOption.REPLACE_EXISTING)

    //Open Excel
    val pkg = OPCPackage.open(new FileInputStream(reportFilePath.toAbsolutePath().toString()))
    val wb = new XSSFWorkbook(pkg);

    (reportFilePath, pkg, wb)
  }

  def finishExcel(reportFilePath: Path, pkg: OPCPackage, wb: XSSFWorkbook) = {
    val out = new FileOutputStream(reportFilePath.toAbsolutePath().toString());
    wb.write(out);
    out.close();
    pkg.close();

    new File(reportFilePath.toAbsolutePath().toString())
  }

  def createDailyReport(monitor: Monitor.Value, reportDate: DateTime, data: DailyReport) = {

    val (reportFilePath, pkg, wb) = prepareTemplate("daily_report.xlsx")

    val sheet = wb.getSheetAt(0)
    val titleRow = sheet.getRow(2)
    val titleCell = titleRow.getCell(0)

    val calColor = titleRow.getCell(8).getCellStyle.getFillBackgroundColor
    val repairColor = titleRow.getCell(9).getCellStyle.getFillBackgroundColor
    val maintanceColor = titleRow.getCell(10).getCellStyle.getFillBackgroundColor
    val invalidColor = titleRow.getCell(11).getCellStyle.getFillBackgroundColor
    val dataLostColor = titleRow.getCell(13).getCellStyle.getFillBackgroundColor
    val defaultStyle = sheet.getRow(6).getCell(1).getCellStyle
    val invalidStyle = sheet.getRow(6).getCell(1).getCellStyle

    titleCell.setCellValue("監測站:" + Monitor.map(monitor).name)
    sheet.getRow(1).getCell(16).setCellValue("查詢日期:" + DateTime.now.toString("YYYY/MM/dd"))
    titleRow.getCell(16).setCellValue("資料日期:" + reportDate.toString("YYYY/MM/dd"))

    for {
      col <- 1 to data.typeList.length
      row <- 4 to 27
      mtRecord = data.typeList(col - 1)
      cell = sheet.getRow(row).getCell(col)
      cellData = mtRecord.dataList(row - 4)
    } {
      val (date, valueOpt, statusOpt) = cellData
      if (valueOpt.isEmpty || statusOpt.isEmpty) {
        cell.setCellValue("-")
        invalidStyle.setFillBackgroundColor(dataLostColor)
        cell.setCellStyle(invalidStyle)
      } else {
        val value = valueOpt.get
        val status = statusOpt.get
        cell.setCellValue(value)
        val style =
          if (MonitorStatus.isCalbration(status)) {
            invalidStyle.setFillBackgroundColor(calColor)
            invalidStyle
          } else if (MonitorStatus.isRepairing(status)) {
            invalidStyle.setFillBackgroundColor(repairColor)
            invalidStyle
          } else if (MonitorStatus.isMaintance(status)) {
            invalidStyle.setFillBackgroundColor(maintanceColor)
            invalidStyle
          } else if (MonitorStatus.isInvalidData(status)) {
            invalidStyle.setFillBackgroundColor(invalidColor)
            invalidStyle
          } else if (MonitorStatus.isDataLost(status)) {
            invalidStyle.setFillBackgroundColor(dataLostColor)
            invalidStyle
          } else if (MonitorStatus.isNormal(status))
            defaultStyle
          else
            invalidStyle

        cell.setCellStyle(style)
      }
    }

    for {
      col <- 1 to data.typeList.length
    } {
      val stat = data.typeList(col - 1).stat
      if (stat.count != 0) {
        sheet.getRow(28).getCell(col).setCellValue(stat.avg)
        sheet.getRow(29).getCell(col).setCellValue(stat.max)
        sheet.getRow(30).getCell(col).setCellValue(stat.min)
        sheet.getRow(31).getCell(col).setCellValue(stat.effectPercent * 100)
      } else {
        sheet.getRow(28).getCell(col).setCellValue("-")
        sheet.getRow(29).getCell(col).setCellValue("-")
        sheet.getRow(30).getCell(col).setCellValue("-")
        sheet.getRow(31).getCell(col).setCellValue("-")
      }
    }

    finishExcel(reportFilePath, pkg, wb)
  }

  def createMonthlyReport(monitor: Monitor.Value, reportDate: DateTime, data: MonthlyReport, nDay: Int) = {
    val (reportFilePath, pkg, wb) = prepareTemplate("monthly_report.xlsx")
    val evaluator = wb.getCreationHelper().createFormulaEvaluator()

    val titleCell = wb.getSheetAt(2).getRow(2)

    val calColor = wb.getSheetAt(2).getRow(2).getCell(17).getCellStyle.getFillBackgroundColor
    val repairColor = wb.getSheetAt(2).getRow(2).getCell(18).getCellStyle.getFillBackgroundColor
    val maintanceColor = wb.getSheetAt(2).getRow(2).getCell(19).getCellStyle.getFillBackgroundColor
    val invalidColor = wb.getSheetAt(2).getRow(2).getCell(20).getCellStyle.getFillBackgroundColor
    val dataLostColor = wb.getSheetAt(2).getRow(2).getCell(21).getCellStyle.getFillBackgroundColor
    val defaultStyle = wb.getSheetAt(2).getRow(4).getCell(1).getCellStyle
    val invalidStyle = wb.getSheetAt(2).getRow(4).getCell(1).getCellStyle

    def fillEffectSheet(sheet: XSSFSheet) = {
      val titleRow = sheet.getRow(2)
      val titleCell = titleRow.getCell(0)
      titleCell.setCellValue("監測站:" + Monitor.map(monitor).name)
      sheet.getRow(1).getCell(16).setCellValue("查詢日期:" + DateTime.now.toString("YYYY/MM/dd"))
      titleRow.getCell(16).setCellValue("資料日期:" + reportDate.toString("YYYY年MM月"))

      for {
        col <- 1 to data.typeArray.length
        row <- 5 to (5 + nDay - 1)
        mtRecord = data.typeArray(col - 1)
        cell = sheet.getRow(row).getCell(col)
        cellData = mtRecord.dataList(row - 5)
      } {
        if (cellData.count == 0)
          cell.setCellValue("-")
        else
          cell.setCellValue(cellData.count)
      }

      for {
        col <- 1 to data.typeArray.length
        mtRecord = data.typeArray(col - 1)
      } {
        val sum = mtRecord.dataList.map(_.count).sum
        sheet.getRow(36).getCell(col).setCellValue(sum)
        sheet.getRow(37).getCell(col).setCellValue(nDay * 24)
        evaluator.evaluateFormulaCell(sheet.getRow(38).getCell(col))
      }

    }

    def fillMonthlySheet(sheet: XSSFSheet) = {
      val titleRow = sheet.getRow(2)
      val titleCell = titleRow.getCell(0)
      titleCell.setCellValue("監測站:" + Monitor.map(monitor).name)
      sheet.getRow(1).getCell(16).setCellValue("查詢日期:" + DateTime.now.toString("YYYY/MM/dd"))
      titleRow.getCell(16).setCellValue("資料日期:" + reportDate.toString("YYYY年MM月"))

      for {
        col <- 1 to data.typeArray.length
        row <- 5 to (5 + nDay - 1)
        mtRecord = data.typeArray(col - 1)
        cell = sheet.getRow(row).getCell(col)
        cellData = mtRecord.dataList(row - 5)
      } {
        if (cellData.count == 0)
          cell.setCellValue("-")
        else
          cell.setCellValue(cellData.avg)
      }

      for {
        col <- 1 to data.typeArray.length
        mtRecord = data.typeArray(col - 1)
      } {
        val stat = mtRecord.stat
        sheet.getRow(36).getCell(col).setCellValue(stat.avg)
        sheet.getRow(37).getCell(col).setCellValue(stat.max)
        sheet.getRow(38).getCell(col).setCellValue(stat.min)
        evaluator.evaluateFormulaCell(sheet.getRow(39).getCell(col))
      }
    }

    def fillMonthlyHourSheet(report: MonthHourReport) = {
      for {
        sheetIndex <- 2 to 19 - 1
        sheet = wb.getSheetAt(sheetIndex)
      } {
        sheet.getRow(1).getCell(0).setCellValue("監測站:" + Monitor.map(monitor).name)
        sheet.getRow(1).getCell(25).setCellValue("查詢日期:" + DateTime.now.toString("YYYY/MM/dd"))
        sheet.getRow(1).getCell(25).setCellValue("資料日期:" + reportDate.toString("YYYY年MM月"))
      }

      for {
        sheetIndex <- 2 to 19 - 1
        sheet = wb.getSheetAt(sheetIndex)
        col <- 1 to 24
        row <- 4 to (4 + nDay - 1)
        dayRecord = report.dailyReports(row - 4)
        cell = sheet.getRow(row).getCell(col)
        cellData = dayRecord.typeList(sheetIndex - 2).dataList(col - 1)
      } {
        val (date, valueOpt, statusOpt) = cellData
        if (valueOpt.isEmpty || statusOpt.isEmpty) {
          cell.setCellValue("-")
          invalidStyle.setFillBackgroundColor(dataLostColor)
          cell.setCellStyle(invalidStyle)
        } else {
          val value = valueOpt.get
          val status = statusOpt.get
          cell.setCellValue(value)
          val style =
            if (MonitorStatus.isCalbration(status)) {
              invalidStyle.setFillBackgroundColor(calColor)
              invalidStyle
            } else if (MonitorStatus.isRepairing(status)) {
              invalidStyle.setFillBackgroundColor(repairColor)
              invalidStyle
            } else if (MonitorStatus.isMaintance(status)) {
              invalidStyle.setFillBackgroundColor(maintanceColor)
              invalidStyle
            } else if (MonitorStatus.isInvalidData(status)) {
              invalidStyle.setFillBackgroundColor(invalidColor)
              invalidStyle
            } else if (MonitorStatus.isDataLost(status)) {
              invalidStyle.setFillBackgroundColor(dataLostColor)
              invalidStyle
            } else if (MonitorStatus.isNormal(status))
              defaultStyle
            else
              invalidStyle

          cell.setCellStyle(style)
        }
      }

      //Day Stat
      for {
        sheetIndex <- 2 to 19 - 1
        sheet = wb.getSheetAt(sheetIndex)
        row <- 4 to (4 + nDay - 1)
        dayRecord = report.dailyReports(row - 4)
        stat = dayRecord.typeList(sheetIndex - 2).stat
      } {
        if (stat.count != 0) {
          sheet.getRow(row).getCell(25).setCellValue(stat.avg)
          sheet.getRow(row).getCell(26).setCellValue(stat.count)
          sheet.getRow(row).getCell(27).setCellValue(stat.max)
          sheet.getRow(row).getCell(28).setCellValue(stat.min)
          sheet.getRow(row).getCell(29).setCellValue(stat.avg * stat.count)
        } else {
          sheet.getRow(row).getCell(25).setCellValue("-")
          sheet.getRow(row).getCell(26).setCellValue(0)
          sheet.getRow(row).getCell(27).setCellValue("-")
          sheet.getRow(row).getCell(28).setCellValue("-")
          sheet.getRow(row).getCell(29).setCellValue(0)
        }
      }

      //Day Stat
      for {
        sheetIndex <- 2 to 19 - 1
        sheet = wb.getSheetAt(sheetIndex)
        col <- 1 to 24
        row <- 4 to (4 + nDay - 1)
        stat = report.hourStatArray(col - 1)
      } {
        if (stat.count != 0) {
          sheet.getRow(35).getCell(col).setCellValue(stat.avg)
          sheet.getRow(36).getCell(col).setCellValue(stat.count)
          sheet.getRow(37).getCell(col).setCellValue(stat.max)
          sheet.getRow(38).getCell(col).setCellValue(stat.min)
          sheet.getRow(39).getCell(col).setCellValue(stat.avg * stat.count)
        } else {
          sheet.getRow(35).getCell(col).setCellValue("-")
          sheet.getRow(36).getCell(col).setCellValue(stat.count)
          sheet.getRow(37).getCell(col).setCellValue("-")
          sheet.getRow(38).getCell(col).setCellValue("-")
          sheet.getRow(39).getCell(col).setCellValue("-")
        }        
      }

      for {
        sheetIndex <- 2 to 19 - 1
        sheet = wb.getSheetAt(sheetIndex)
      }{
        evaluator.evaluateFormulaCell(sheet.getRow(35).getCell(25))
        evaluator.evaluateFormulaCell(sheet.getRow(36).getCell(26))
        evaluator.evaluateFormulaCell(sheet.getRow(37).getCell(27))
        evaluator.evaluateFormulaCell(sheet.getRow(38).getCell(28))
        evaluator.evaluateFormulaCell(sheet.getRow(39).getCell(29))
      }
      
      //Re-evaluate
      for {
        sheetIndex <- 2 to 19 - 1
        sheet = wb.getSheetAt(sheetIndex)
        col <- 1 to (1 + nDay - 1)        
      } {
        evaluator.evaluateFormulaCell(sheet.getRow(45).getCell(col))
        evaluator.evaluateFormulaCell(sheet.getRow(46).getCell(col))
        evaluator.evaluateFormulaCell(sheet.getRow(47).getCell(col))
      }
    }

    def fillGraphHourSheet(report: MonthHourReport) = {
      val sheet = wb.getSheetAt(19)
      var row_start = 1
      for {
        dayReport <- report.dailyReports
      } {
        for {
          mt_i <- 0 to dayReport.typeList.length - 1
          dataI <- dayReport.typeList(mt_i).dataList.zipWithIndex
        } {
          val (data, idx) = dataI
          if (data._2.isDefined) {
            Logger.debug("data._2=" + data._2)
            Logger.debug("row_start=" + row_start)
            Logger.debug("idx=" + idx)
            if (sheet.getRow(row_start + idx) == null)
              sheet.createRow(row_start + idx).createCell(mt_i + 1).setCellValue(data._2.get)
            else
              sheet.getRow(row_start + idx).createCell(mt_i + 1).setCellValue(data._2.get)
          }

          if (mt_i == 0) {
            if (sheet.getRow(row_start + idx) == null)
              sheet.createRow(row_start + idx).createCell(0).setCellValue(data._1.toDateTime().toString("YY-MM-DD HH:mm"))
            else
              sheet.getRow(row_start + idx).createCell(0).setCellValue(data._1.toDateTime().toString("YY-MM-DD HH:mm"))
          }
        }
        row_start += dayReport.typeList(0).dataList.length
      }
    }
    
    // 有效率月報
    fillEffectSheet(wb.getSheetAt(0))
    fillMonthlySheet(wb.getSheetAt(1))
    val monthlyHourReport = monthlyHourReportHelper(monitor, reportDate)
    fillMonthlyHourSheet(monthlyHourReport)
    fillGraphHourSheet(monthlyHourReport)
    
    finishExcel(reportFilePath, pkg, wb)
  }
}