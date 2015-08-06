package controllers
import play.api._
import play.api.Play.current
import org.apache.poi.openxml4j.opc._
import org.apache.poi.xssf.usermodel._
import controllers.Report._
import models._
import models.Record._
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

    val calStyle = titleRow.getCell(8).getCellStyle
    val repairStyle = titleRow.getCell(9).getCellStyle
    val maintanceStyle = titleRow.getCell(10).getCellStyle
    val invalidStyle = titleRow.getCell(11).getCellStyle
    val dataLostStyle = titleRow.getCell(13).getCellStyle
    val defaultStyle = titleCell.getCellStyle
    
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
        cell.setCellStyle(dataLostStyle)
      } else {
        val value = valueOpt.get
        val status = statusOpt.get
        cell.setCellValue(value)
        val style =
          if (MonitorStatus.isCalbration(status)) {
            calStyle
          } else if (MonitorStatus.isRepairing(status)) {
            repairStyle
          } else if (MonitorStatus.isMaintance(status)) {
            maintanceStyle
          } else if (MonitorStatus.isInvalidData(status)) {
            invalidStyle
          } else if (MonitorStatus.isDataLost(status)) {
            dataLostStyle
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

    val calStyle = wb.getSheetAt(2).getRow(2).getCell(17).getCellStyle
    val repairStyle = wb.getSheetAt(2).getRow(2).getCell(18).getCellStyle
    val maintanceStyle = wb.getSheetAt(2).getRow(2).getCell(19).getCellStyle
    val invalidStyle = wb.getSheetAt(2).getRow(2).getCell(20).getCellStyle
    val dataLostStyle = wb.getSheetAt(2).getRow(2).getCell(21).getCellStyle
    val defaultStyle = wb.getSheetAt(2).getRow(4).getCell(1).getCellStyle


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
          cell.setCellStyle(dataLostStyle)
        } else {
          val value = valueOpt.get
          val status = statusOpt.get
          cell.setCellValue(value)
          val style =
            if (MonitorStatus.isCalbration(status)) {
              calStyle
            } else if (MonitorStatus.isRepairing(status)) {
              repairStyle
            } else if (MonitorStatus.isMaintance(status)) {
              maintanceStyle
            } else if (MonitorStatus.isInvalidData(status)) {
              invalidStyle
            } else if (MonitorStatus.isDataLost(status)) {
              dataLostStyle
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

  def createYearlyReport(monitor: Monitor.Value, reportDate: DateTime, report: IntervalReport) = {
    val (reportFilePath, pkg, wb) = prepareTemplate("yearly_report.xlsx")
    val evaluator = wb.getCreationHelper().createFormulaEvaluator()
    val sheet = wb.getSheetAt(0)
    sheet.getRow(2).getCell(0).setCellValue("監測站:" + Monitor.map(monitor).name)
    sheet.getRow(1).getCell(16).setCellValue("查詢日期:" + DateTime.now.toString("YYYY/MM/dd"))
    sheet.getRow(2).getCell(16).setCellValue("資料日期:" + reportDate.toString("YYYY年"))

    for{
      row <- 4 to 4+12-1
      col <- 1 to 17
      data = report.typeArray(col-1).dataList(row-4)
    }{
      if(data.count != 0)
        sheet.getRow(row).getCell(col).setCellValue(data.avg)
      else
        sheet.getRow(row).getCell(col).setCellValue("-")
    }
    
    for{
      col <- 1 to 17
      stat = report.typeArray(col-1).stat
    }{
      if(stat.count != 0){
        sheet.getRow(16).getCell(col).setCellValue(stat.avg)
        sheet.getRow(17).getCell(col).setCellValue(stat.max)
        sheet.getRow(18).getCell(col).setCellValue(stat.min)
        sheet.getRow(19).getCell(col).setCellValue(stat.effectPercent*100)
      }else{
        sheet.getRow(16).getCell(col).setCellValue("-")
        sheet.getRow(17).getCell(col).setCellValue("-")
        sheet.getRow(18).getCell(col).setCellValue("-")
        sheet.getRow(19).getCell(col).setCellValue(0)        
      }
    }
    
    finishExcel(reportFilePath, pkg, wb)
  }

  def createSingleSiteEffectiveReport(monitor: Monitor.Value, reportDate: DateTime, rateList: List[MonitorEffectiveRate], statMap: Map[MonitorType.Value, Stat]) = {
    val (reportFilePath, pkg, wb) = prepareTemplate("effective_single.xlsx")
    val evaluator = wb.getCreationHelper().createFormulaEvaluator()

    val sheet = wb.getSheetAt(0)
    sheet.getRow(2).getCell(0).setCellValue("監測站:" + Monitor.map(monitor).name)
    sheet.getRow(1).getCell(16).setCellValue("查詢日期:" + DateTime.now.toString("YYYY/MM/dd"))
    sheet.getRow(2).getCell(16).setCellValue("資料日期:" + reportDate.toString("YYYY年"))

    for{
      row <- 4 to 4+12-1
      mt <- MonitorType.monitorReportList.zipWithIndex
      data = rateList(row-4)
      cell = sheet.getRow(row).getCell(mt._2 + 1)
    }{
      cell.setCellValue(data.rateMap(mt._1)*100)
    }
    
    for{
      mt <- MonitorType.monitorReportList.zipWithIndex
      stat = statMap(mt._1)
    }{
      sheet.getRow(16).getCell(mt._2 + 1).setCellValue(stat.min*100)
      sheet.getRow(17).getCell(mt._2 + 1).setCellValue(stat.max*100)
      sheet.getRow(18).getCell(mt._2 + 1).setCellValue(stat.avg*100)
    }
    finishExcel(reportFilePath, pkg, wb)
  }

  def createMultipleSiteEffectiveReport(monitorType: MonitorType.Value, reportDate: DateTime, rateList: List[MonitorTypeEffectiveRate], statMap: Map[Monitor.Value, Stat]) = {
    val (reportFilePath, pkg, wb) = prepareTemplate("effective_mulitiple.xlsx")
    val evaluator = wb.getCreationHelper().createFormulaEvaluator()

    val sheet = wb.getSheetAt(0)
    sheet.getRow(2).getCell(0).setCellValue("測項名稱:" + MonitorType.map(monitorType).desp)
    sheet.getRow(1).getCell(11).setCellValue("查詢日期:" + DateTime.now.toString("YYYY/MM/dd"))
    sheet.getRow(2).getCell(11).setCellValue("資料日期:" + reportDate.toString("YYYY年"))

    for {
      m <- Monitor.mvList.zipWithIndex
      cell = sheet.getRow(3).getCell(m._2 + 1)
    } {
      cell.setCellValue(Monitor.map(m._1).name)
    }

    for {
      row <- 4 to 4 + 12 - 1
      m <- Monitor.mvList.zipWithIndex
      data = rateList(row - 4).rateMap(m._1)
      cell = sheet.getRow(row).getCell(m._2+1)
    } {
      cell.setCellValue(data*100)      
    }

    for {
      m <- Monitor.mvList.zipWithIndex
      stat = statMap(m._1)
    }{
      sheet.getRow(16).getCell(m._2+1).setCellValue(stat.min * 100)
      sheet.getRow(17).getCell(m._2+1).setCellValue(stat.max * 100)
      sheet.getRow(18).getCell(m._2+1).setCellValue(stat.avg * 100)
    }
    
    finishExcel(reportFilePath, pkg, wb)
  }

  def psiDailyReport(monitor: Monitor.Value, reportDate: DateTime, psiHourRecords: List[(Option[Float], Map[MonitorType.Value, (Option[Float], Option[Float])])]) = {
    val (reportFilePath, pkg, wb) = prepareTemplate("psi_daily.xlsx")
    val evaluator = wb.getCreationHelper().createFormulaEvaluator()

    val sheet = wb.getSheetAt(0)
    sheet.getRow(2).getCell(0).setCellValue("測站:" + Monitor.map(monitor).name)
    sheet.getRow(1).getCell(10).setCellValue("查詢日期:" + DateTime.now.toString("YYYY/MM/dd"))
    sheet.getRow(2).getCell(10).setCellValue("資料日期:" + reportDate.toString("YYYY年MM月dd日"))

    for {
      mtv <- MonitorType.psiList.zipWithIndex
    } {
      sheet.getRow(3).getCell(2 + mtv._2 * 2).setCellValue(MonitorType.map(mtv._1).desp)
    }

    import org.apache.poi.ss.usermodel._

    val greenStyle = sheet.getRow(31).getCell(0).getCellStyle
    val yellowStyle = sheet.getRow(31).getCell(1).getCellStyle
    val violetStyle = sheet.getRow(31).getCell(2).getCellStyle
    val brownStyle = sheet.getRow(31).getCell(3).getCellStyle

    for {
      row <- 5 to 5 + 24 - 1
      data = psiHourRecords(row - 5)
    } {
      if (data._1.isDefined) {
        val cell = sheet.getRow(row).getCell(1)
        val v = data._1.get
        cell.setCellValue(v)
        if (v < 50)
          cell.setCellStyle(greenStyle)
        else if (v <= 100)
          cell.setCellStyle(yellowStyle)
        else if (v < 200)
          cell.setCellStyle(violetStyle)
        else
          cell.setCellStyle(brownStyle)
      } else
        sheet.getRow(row).getCell(1).setCellValue("-")

      for {
        mtv <- MonitorType.psiList.zipWithIndex
        psi = data._2(mtv._1)
      } {
        if(psi._1.isDefined)
          sheet.getRow(row).getCell(2 + mtv._2*2+1).setCellValue(psi._1.get)
        else
          sheet.getRow(row).getCell(2 + mtv._2*2+1).setCellValue("-")
          
        if(psi._2.isDefined)
          sheet.getRow(row).getCell(2 + mtv._2*2).setCellValue(psi._2.get)
        else
          sheet.getRow(row).getCell(2 + mtv._2*2).setCellValue("-")
      }
    }

    finishExcel(reportFilePath, pkg, wb)
  }
  
  import models.Realtime._
  def psiMonthlyReport(monitor: Monitor.Value, reportDate:DateTime, psiDailyList: List[PsiReport], nDays: Int)={
    val (reportFilePath, pkg, wb) = prepareTemplate("psi_monthly.xlsx")
    val evaluator = wb.getCreationHelper().createFormulaEvaluator()

    val sheet = wb.getSheetAt(0)
    sheet.getRow(2).getCell(0).setCellValue("測站:" + Monitor.map(monitor).name)
    sheet.getRow(1).getCell(10).setCellValue("查詢日期:" + DateTime.now.toString("YYYY/MM/dd"))
    sheet.getRow(2).getCell(10).setCellValue("資料日期:" + reportDate.toString("YYYY年MM月"))
    for {
      mtv <- MonitorType.psiList.zipWithIndex
    } {
      sheet.getRow(3).getCell(2 + mtv._2 * 2).setCellValue(MonitorType.map(mtv._1).desp)
    }

    val greenStyle = sheet.getRow(38).getCell(0).getCellStyle
    val yellowStyle = sheet.getRow(38).getCell(1).getCellStyle
    val violetStyle = sheet.getRow(38).getCell(2).getCellStyle
    val brownStyle = sheet.getRow(38).getCell(3).getCellStyle

    for {
      row <- 5 to 5 + nDays - 1
      data = psiDailyList(row - 5)
    } {
      if (data.psi.isDefined) {
        val cell = sheet.getRow(row).getCell(1)
        val v = data.psi.get
        cell.setCellValue(v)
        if (v < 50)
          cell.setCellStyle(greenStyle)
        else if (v <= 100)
          cell.setCellStyle(yellowStyle)
        else if (v < 200)
          cell.setCellStyle(violetStyle)
        else
          cell.setCellStyle(brownStyle)
      } else
        sheet.getRow(row).getCell(1).setCellValue("-")

      for {
        mtv <- MonitorType.psiList.zipWithIndex
        psi = data.sub_map(mtv._1)
      } {
        if(psi._1.isDefined)
          sheet.getRow(row).getCell(2 + mtv._2*2+1).setCellValue(psi._1.get)
        else
          sheet.getRow(row).getCell(2 + mtv._2*2+1).setCellValue("-")
          
        if(psi._2.isDefined)
          sheet.getRow(row).getCell(2 + mtv._2*2).setCellValue(psi._2.get)
        else
          sheet.getRow(row).getCell(2 + mtv._2*2).setCellValue("-")
      }
    }

    finishExcel(reportFilePath, pkg, wb)
  }

  def epaCompareReport(monitor: Monitor.Value, epaMonitor: EpaMonitor.Value, reportDate: DateTime, myMap: Map[MonitorType.Value, (Map[DateTime, (Option[Float], Option[String])], Stat)], epaMap: Map[MonitorType.Value, (Map[DateTime, EpaHourRecord], Stat)], hours: List[DateTime]) = {
    val (reportFilePath, pkg, wb) = prepareTemplate("epa_compare.xlsx")
    val evaluator = wb.getCreationHelper().createFormulaEvaluator()

    val sheet = wb.getSheetAt(0)
    sheet.getRow(2).getCell(0).setCellValue("測站:" + Monitor.map(monitor).name + "/環保署測站:" + EpaMonitor.map(epaMonitor).name)
    sheet.getRow(1).getCell(24).setCellValue("查詢日期:" + DateTime.now.toString("YYYY/MM/dd"))
    sheet.getRow(2).getCell(24).setCellValue("資料日期:" + reportDate.toString("YYYY年MM月dd"))

    val calStyle = wb.getSheetAt(0).getRow(2).getCell(10).getCellStyle
    val repairStyle = wb.getSheetAt(0).getRow(2).getCell(12).getCellStyle
    val maintanceStyle = wb.getSheetAt(0).getRow(2).getCell(14).getCellStyle
    val invalidStyle = wb.getSheetAt(0).getRow(2).getCell(16).getCellStyle
    val dataLostStyle = wb.getSheetAt(0).getRow(2).getCell(18).getCellStyle
    val defaultStyle = wb.getSheetAt(0).getRow(5).getCell(2).getCellStyle

    for {
      mt <- MonitorType.epaReportList.zipWithIndex
      row = mt._2 * 2 + 5
    } {
      sheet.getRow(row).getCell(1).setCellValue(Monitor.map(monitor).name)
      sheet.getRow(row+1).getCell(1).setCellValue(EpaMonitor.map(epaMonitor).name)      
      for {
        hr <- hours.zipWithIndex
        col = hr._2 + 2
        cell = sheet.getRow(row).getCell(col)
      } {
        val vOpt = myMap(mt._1)._1.get(hr._1)        
        if (vOpt.isDefined) {
          val p = vOpt.get          
          cell.setCellValue(p._1.get)
          val status = p._2.get
          val style =
            if (MonitorStatus.isCalbration(status)) {
              calStyle
            } else if (MonitorStatus.isRepairing(status)) {
              repairStyle
            } else if (MonitorStatus.isMaintance(status)) {
              maintanceStyle
            } else if (MonitorStatus.isInvalidData(status)) {
              invalidStyle
            } else if (MonitorStatus.isDataLost(status)) {
              dataLostStyle
            } else if (MonitorStatus.isNormal(status))
              defaultStyle
            else
              invalidStyle
          cell.setCellStyle(style)
        }else{
          cell.setCellValue("-")
          cell.setCellStyle(dataLostStyle)
        }        
      }
      
      if(myMap(mt._1)._2.count !=0){
        val stat = myMap(mt._1)._2
        sheet.getRow(row).getCell(26).setCellValue(stat.min)
        sheet.getRow(row).getCell(27).setCellValue(stat.max)
        sheet.getRow(row).getCell(28).setCellValue(stat.avg)
      }else{
        sheet.getRow(row).getCell(26).setCellValue("-")
        sheet.getRow(row).getCell(27).setCellValue("-")
        sheet.getRow(row).getCell(28).setCellValue("-")
      }
      
      for {
        hr <- hours.zipWithIndex
        col = hr._2 + 2
        cell = sheet.getRow(row+1).getCell(col)
      } {
         val vOpt = epaMap(mt._1)._1.get(hr._1) 
         if(vOpt.isEmpty){
           cell.setCellValue("-")
         }else{
           val epaRecord = vOpt.get
           cell.setCellValue(epaRecord.value)
         }
      } 
      
      if(epaMap(mt._1)._2.count !=0){
        val stat = epaMap(mt._1)._2
        sheet.getRow(row+1).getCell(26).setCellValue(stat.min)
        sheet.getRow(row+1).getCell(27).setCellValue(stat.max)
        sheet.getRow(row+1).getCell(28).setCellValue(stat.avg)
      }else{
        sheet.getRow(row+1).getCell(26).setCellValue("-")
        sheet.getRow(row+1).getCell(27).setCellValue("-")
        sheet.getRow(row+1).getCell(28).setCellValue("-")
      }      
    }
    finishExcel(reportFilePath, pkg, wb)
  }
}