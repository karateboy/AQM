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
import org.apache.poi.ss.usermodel._

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

  def createStyle(mt: MonitorType.Value)(implicit wb: XSSFWorkbook) = {
    val prec = MonitorType.map(mt).prec
    val format_str = "0." + "0" * prec
    val style = wb.createCellStyle();
    val format = wb.createDataFormat();
    // Create a new font and alter it.
    val font = wb.createFont();
    font.setFontHeightInPoints(10);
    font.setFontName("標楷體");

    style.setFont(font)
    style.setDataFormat(format.getFormat(format_str))
    style.setBorderBottom(CellStyle.BORDER_THIN);
    style.setBottomBorderColor(IndexedColors.BLACK.getIndex());
    style.setBorderLeft(CellStyle.BORDER_THIN);
    style.setLeftBorderColor(IndexedColors.BLACK.getIndex());
    style.setBorderRight(CellStyle.BORDER_THIN);
    style.setRightBorderColor(IndexedColors.BLACK.getIndex());
    style.setBorderTop(CellStyle.BORDER_THIN);
    style.setTopBorderColor(IndexedColors.BLACK.getIndex());
    style
  }

  def createColorStyle(fgColors: Array[XSSFColor], mt: MonitorType.Value)(implicit wb: XSSFWorkbook) = {
    fgColors.map {
      color =>
        val style = createStyle(mt)
        style.setFillForegroundColor(color)
        style.setFillPattern(FillPatternType.SOLID_FOREGROUND)
        style
    }
  }

  def getStyle(tag: String, normalStyle: XSSFCellStyle, abnormalStyles: Array[XSSFCellStyle]) = {
    import MonitorStatus._
    val info = MonitorStatus.getTagInfo(tag)
    info.statusType match {
      case StatusType.Internal =>
        {
          if (isNormalStat(tag))
            normalStyle
          else if (isCalbration(tag))
            abnormalStyles(0)
          else if (isRepairing(tag))
            abnormalStyles(1)
          else if (isMaintance(tag))
            abnormalStyles(2)
          else
            abnormalStyles(3)
        }
      case StatusType.Auto =>
        if (SystemConfig.getConfig(SystemConfig.AutoAuditAsNormal, "True").toBoolean)
          normalStyle
        else
          abnormalStyles(3)
      case StatusType.Manual =>
        abnormalStyles(4)
    }
  }

  def createAllDailyReport(reportDate: DateTime) = {
    implicit val (reportFilePath, pkg, wb) = prepareTemplate("all_daily_report.xlsx")
    val format = wb.createDataFormat();
    val sheet = wb.getSheetAt(0)
    val titleRow = sheet.getRow(2)
    val titleCell = titleRow.getCell(0)

    val fgColors =
      {
        val seqColors =
          for (col <- 3 to 7)
            yield titleRow.getCell(col).getCellStyle.getFillForegroundXSSFColor
        seqColors.toArray
      }

    def fillMonitorDailyReport(monitor: Monitor.Value, data: DailyReport, sheetIdx: Int) = {
      val sheet = wb.getSheetAt(sheetIdx)
      val titleRow = sheet.getRow(2)
      val titleCell = titleRow.getCell(0)

      titleCell.setCellValue("監測站:" + Monitor.map(monitor).name)
      sheet.getRow(1).getCell(27).setCellValue("查詢日期:" + DateTime.now.toString("YYYY/MM/dd"))
      titleRow.getCell(27).setCellValue("資料日期:" + reportDate.toString("YYYY/MM/dd"))

      for {
        col <- 1 to data.typeList.length
        mtRecord = data.typeList(col - 1)
        normalStyle = createStyle(mtRecord.monitorType)
        abnormalStyles = createColorStyle(fgColors, mtRecord.monitorType)
        row <- 4 to 27
        cell = sheet.getRow(row).getCell(col)
        cellData = mtRecord.dataList(row - 4)
      } {
        val (date, valueOpt, statusOpt) = cellData
        if (valueOpt.isEmpty || statusOpt.isEmpty) {
          cell.setCellValue("-")
        } else {
          val value = valueOpt.get
          val status = statusOpt.get
          cell.setCellValue(value)

          val cellStyle = getStyle(status, normalStyle, abnormalStyles)
          cell.setCellStyle(cellStyle)
        }
      }

      for {
        col <- 1 to data.typeList.length
      } {
        val stat = data.typeList(col - 1).stat
        if (stat.count != 0) {
          sheet.getRow(28).getCell(col).setCellValue(stat.avg.get)
          sheet.getRow(29).getCell(col).setCellValue(stat.max.get)
          sheet.getRow(30).getCell(col).setCellValue(stat.min.get)
          sheet.getRow(31).getCell(col).setCellValue(stat.effectPercent.get * 100)
        } else {
          sheet.getRow(28).getCell(col).setCellValue("-")
          sheet.getRow(29).getCell(col).setCellValue("-")
          sheet.getRow(30).getCell(col).setCellValue("-")
          sheet.getRow(31).getCell(col).setCellValue("-")
        }
      }

      //Hide col not in use
      for {
        col <- 1 to data.typeList.length
      } {
        val mt = data.typeList(col - 1).monitorType
        if (!Monitor.map(monitor).monitorTypes.contains(mt)) {
          sheet.setColumnHidden(col, true)
        }
      }

    }
    for {
      (monitor, sheetIdx) <- Monitor.mvList.zipWithIndex
      dailyReport = Record.getDailyReport(monitor, reportDate)
    } {
      wb.setSheetName(sheetIdx, Monitor.map(monitor).name)
      fillMonitorDailyReport(monitor, dailyReport, sheetIdx)      
    }
    wb.setActiveSheet(0)
    finishExcel(reportFilePath, pkg, wb)
  }

  def createDailyReport(monitor: Monitor.Value, reportDate: DateTime, data: DailyReport) = {

    implicit val (reportFilePath, pkg, wb) = prepareTemplate("daily_report.xlsx")
    val format = wb.createDataFormat();
    val sheet = wb.getSheetAt(0)
    val titleRow = sheet.getRow(2)
    val titleCell = titleRow.getCell(0)

    val fgColors =
      {
        val seqColors =
          for (col <- 3 to 7)
            yield titleRow.getCell(col).getCellStyle.getFillForegroundXSSFColor
        seqColors.toArray
      }

    titleCell.setCellValue("監測站:" + Monitor.map(monitor).name)
    sheet.getRow(1).getCell(19).setCellValue("查詢日期:" + DateTime.now.toString("YYYY/MM/dd"))
    titleRow.getCell(19).setCellValue("資料日期:" + reportDate.toString("YYYY/MM/dd"))

    for {
      col <- 1 to data.typeList.length
      mtRecord = data.typeList(col - 1)
      normalStyle = createStyle(mtRecord.monitorType)
      abnormalStyles = createColorStyle(fgColors, mtRecord.monitorType)
      row <- 4 to 27
      cell = sheet.getRow(row).getCell(col)
      cellData = mtRecord.dataList(row - 4)
    } {
      val (date, valueOpt, statusOpt) = cellData
      if (valueOpt.isEmpty || statusOpt.isEmpty) {
        cell.setCellValue("-")
      } else {
        val value = valueOpt.get
        val status = statusOpt.get
        cell.setCellValue(value)

        val cellStyle = getStyle(status, normalStyle, abnormalStyles)
        cell.setCellStyle(cellStyle)
      }
    }

    for {
      col <- 1 to data.typeList.length
    } {
      val stat = data.typeList(col - 1).stat
      if (stat.count != 0) {
        sheet.getRow(28).getCell(col).setCellValue(stat.avg.get)
        sheet.getRow(29).getCell(col).setCellValue(stat.max.get)
        sheet.getRow(30).getCell(col).setCellValue(stat.min.get)
        sheet.getRow(31).getCell(col).setCellValue(stat.effectPercent.get * 100)
      } else {
        sheet.getRow(28).getCell(col).setCellValue("-")
        sheet.getRow(29).getCell(col).setCellValue("-")
        sheet.getRow(30).getCell(col).setCellValue("-")
        sheet.getRow(31).getCell(col).setCellValue("-")
      }
    }

    //Hide col not in use
    for {
      col <- 1 to data.typeList.length
    } {
      val mt = data.typeList(col - 1).monitorType
      if (!Monitor.map(monitor).monitorTypes.contains(mt)) {
        sheet.setColumnHidden(col, true)
      }
    }

    finishExcel(reportFilePath, pkg, wb)
  }

  def createMonthlyReport(monitor: Monitor.Value, reportDate: DateTime, data: MonthlyReport, nDay: Int) = {
    implicit val (reportFilePath, pkg, wb) = prepareTemplate("monthly_report.xlsx")
    val evaluator = wb.getCreationHelper().createFormulaEvaluator()

    val fgColors =
      {
        val seqColors =
          for (col <- 17 to 21)
            yield wb.getSheetAt(2).getRow(2).getCell(col).getCellStyle.getFillForegroundXSSFColor
        seqColors.toArray
      }

    def fillEffectSheet(sheet: XSSFSheet) = {
      val titleRow = sheet.getRow(2)
      val titleCell = titleRow.getCell(0)
      titleCell.setCellValue("監測站:" + Monitor.map(monitor).name)
      sheet.getRow(1).getCell(17).setCellValue("查詢日期:" + DateTime.now.toString("YYYY/MM/dd"))
      titleRow.getCell(17).setCellValue("資料日期:" + reportDate.toString("YYYY年MM月"))

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
      //Hide unused Monitor Type
      for {
        col <- 1 to data.typeArray.length
        mtRecord = data.typeArray(col - 1)
      }{
        if(!Monitor.map(monitor).monitorTypes.contains(mtRecord.monitorType))
          sheet.setColumnHidden(col, true)          
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

      //Hide col not in use
      for {
        col <- 1 to data.typeArray.length
      } {
        val mt = data.typeArray(col - 1).monitorType
        if (!Monitor.map(monitor).monitorTypes.contains(mt)) {
          sheet.setColumnHidden(col, true)
        }
      }
    }

    // Fill Graph title
    val graph_list = List(MonitorType.C211, MonitorType.A222, MonitorType.A224, MonitorType.A293,
      MonitorType.A225, MonitorType.A214, MonitorType.A296, MonitorType.A213)

    def fillMonthlySheet(sheet: XSSFSheet) = {
      val titleRow = sheet.getRow(2)
      val titleCell = titleRow.getCell(0)
      titleCell.setCellValue("監測站:" + Monitor.map(monitor).name)
      sheet.getRow(1).getCell(22).setCellValue("查詢日期:" + DateTime.now.toString("YYYY/MM/dd"))
      titleRow.getCell(22).setCellValue("資料日期:" + reportDate.toString("YYYY年MM月"))

      val abnormalColor =
        {
          val seqColors =
            for (col <- 3 to 3)
              yield titleRow.getCell(col).getCellStyle.getFillForegroundXSSFColor
          seqColors.toArray
        }

      for {
        col <- 1 to data.typeArray.length
        mtRecord = data.typeArray(col - 1)
        normalStyle = createStyle(mtRecord.monitorType)
        abnormalStyles = createColorStyle(abnormalColor, mtRecord.monitorType)
        row <- 5 to (5 + nDay - 1)
        cell = sheet.getRow(row).getCell(col)
        cellData = mtRecord.dataList(row - 5)
      } {
        if (cellData.count == 0)
          cell.setCellValue("-")
        else {
          cell.setCellValue(cellData.avg.get)
          if (cellData.effectPercent.isDefined && cellData.effectPercent.get >= 0.75)
            cell.setCellStyle(normalStyle)
          else
            cell.setCellStyle(abnormalStyles(0))
        }
      }

      for {
        col <- 1 to data.typeArray.length
        mtRecord = data.typeArray(col - 1)
      } {
        val stat = mtRecord.stat
        
        if (stat.count >= 1) {
          sheet.getRow(36).getCell(col).setCellValue(stat.avg.get)
          sheet.getRow(37).getCell(col).setCellValue(stat.max.get)
          sheet.getRow(38).getCell(col).setCellValue(stat.min.get)
        } else {
          sheet.getRow(36).getCell(col).setCellValue("-")
          sheet.getRow(37).getCell(col).setCellValue("-")
          sheet.getRow(38).getCell(col).setCellValue("-")
        }
        evaluator.evaluateFormulaCell(sheet.getRow(39).getCell(col))
        
      }

      //Hide col not in use
      for {
        col <- 1 to data.typeArray.length
      } {
        val mt = data.typeArray(col - 1).monitorType
        if (!Monitor.map(monitor).monitorTypes.contains(mt)) {
          sheet.setColumnHidden(col, true)
        }
      }

      for {
        mt <- graph_list.zipWithIndex
        row = sheet.getRow(43)
      } {
        val mtCase = MonitorType.map(mt._1)
        val title =
          if (!Monitor.map(monitor).monitorTypes.contains(mt._1))
            s"${Monitor.map(monitor).name}無${mtCase.desp}測項"
          else if (mtCase.std_law.isDefined)
            s"${Monitor.map(monitor).name}${mtCase.desp}小時趨勢圖 (法規:${mtCase.std_law.get}${mtCase.unit})"
          else
            s"${Monitor.map(monitor).name}${mtCase.desp}小時趨勢圖 "

        row.getCell(mt._2).setCellValue(title)
      }
    }

    def fillMonthlyHourSheet(report: MonthHourReport) = {
      val reportTypeLen = report.dailyReports(0).typeList.length

      for {
        sheetIndex <- 2 to reportTypeLen + 1
        sheet = wb.getSheetAt(sheetIndex)
        mt = report.dailyReports(0).typeList(sheetIndex - 2).monitorType
      } {
        if (!Monitor.map(monitor).monitorTypes.contains(mt)) {
          wb.setSheetHidden(sheetIndex, true)
        }
      }

      for {
        sheetIndex <- 2 to reportTypeLen + 1
        sheet = wb.getSheetAt(sheetIndex)
      } {
        sheet.getRow(1).getCell(0).setCellValue("監測站:" + Monitor.map(monitor).name)
        sheet.getRow(1).getCell(25).setCellValue("查詢日期:" + DateTime.now.toString("YYYY/MM/dd"))
        sheet.getRow(2).getCell(25).setCellValue("資料日期:" + reportDate.toString("YYYY年MM月"))
        sheet.getRow(43).getCell(0).setCellValue("資料日期:" + reportDate.toString("YYYY年MM月"))
      }

      for {
        sheetIndex <- 2 to reportTypeLen + 1
        sheet = wb.getSheetAt(sheetIndex)
        mt = report.dailyReports(0).typeList(sheetIndex - 2).monitorType
        normalStyle = createStyle(mt)
        abnormalStyles = createColorStyle(fgColors, mt)
        row <- 4 to (4 + nDay - 1)
        dayRecord = report.dailyReports(row - 4)
        col <- 1 to 24
        cell = sheet.getRow(row).getCell(col)
        cellData = dayRecord.typeList(sheetIndex - 2).dataList(col - 1)
      } {
        val (date, valueOpt, statusOpt) = cellData
        if (valueOpt.isEmpty || statusOpt.isEmpty) {
          cell.setCellValue("-")
        } else {
          cell.setCellValue(valueOpt.get)
          val style = getStyle(statusOpt.get, normalStyle, abnormalStyles)
          cell.setCellStyle(style)
        }
      }

      //Day Stat
      for {
        sheetIndex <- 2 to reportTypeLen + 1
        sheet = wb.getSheetAt(sheetIndex)
        row <- 4 to (4 + nDay - 1)
        dayRecord = report.dailyReports(row - 4)
        stat = dayRecord.typeList(sheetIndex - 2).stat
      } {
        if (stat.count != 0) {
          sheet.getRow(row).getCell(25).setCellValue(stat.avg.get)
          sheet.getRow(row).getCell(26).setCellValue(stat.count)
          sheet.getRow(row).getCell(27).setCellValue(stat.max.get)
          sheet.getRow(row).getCell(28).setCellValue(stat.min.get)
          sheet.getRow(row).getCell(29).setCellValue(stat.avg.get * stat.count)
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
        sheetIndex <- 2 to reportTypeLen + 1
        sheet = wb.getSheetAt(sheetIndex)
        col <- 1 to 24
        row <- 4 to (4 + nDay - 1)
        stat = report.hourStatArray(col - 1)
      } {
        if (stat.count != 0) {
          evaluator.evaluateFormulaCell(sheet.getRow(35).getCell(col))
          sheet.getRow(36).getCell(col).setCellValue(stat.count)
          evaluator.evaluateFormulaCell(sheet.getRow(37).getCell(col))
          evaluator.evaluateFormulaCell(sheet.getRow(38).getCell(col))
          evaluator.evaluateFormulaCell(sheet.getRow(39).getCell(col))
        } else {
          sheet.getRow(35).getCell(col).setCellValue("-")
          sheet.getRow(36).getCell(col).setCellValue(stat.count)
          sheet.getRow(37).getCell(col).setCellValue("-")
          sheet.getRow(38).getCell(col).setCellValue("-")
          sheet.getRow(39).getCell(col).setCellValue("-")
        }
      }

      for {
        sheetIndex <- 2 to reportTypeLen + 1
        sheet = wb.getSheetAt(sheetIndex)
      } {
        evaluator.evaluateFormulaCell(sheet.getRow(35).getCell(25))
        evaluator.evaluateFormulaCell(sheet.getRow(36).getCell(25))
        evaluator.evaluateFormulaCell(sheet.getRow(37).getCell(25))
        evaluator.evaluateFormulaCell(sheet.getRow(38).getCell(25))
        evaluator.evaluateFormulaCell(sheet.getRow(39).getCell(25))

        evaluator.evaluateFormulaCell(sheet.getRow(36).getCell(26))
        evaluator.evaluateFormulaCell(sheet.getRow(37).getCell(27))
        evaluator.evaluateFormulaCell(sheet.getRow(38).getCell(28))
        evaluator.evaluateFormulaCell(sheet.getRow(39).getCell(29))
      }

      //Re-evaluate
      for {
        sheetIndex <- 2 to reportTypeLen + 1
        sheet = wb.getSheetAt(sheetIndex)
        col <- 0 to (1 + nDay)
      } {
        evaluator.evaluateFormulaCell(sheet.getRow(45).getCell(col))
        evaluator.evaluateFormulaCell(sheet.getRow(46).getCell(col))
        evaluator.evaluateFormulaCell(sheet.getRow(47).getCell(col))
      }

      //Fill graph title

    }

    def fillGraphHourSheet(report: MonthHourReport, idx: Int) = {
      val sheet = wb.getSheetAt(idx)
      val graph_idx = graph_list.zipWithIndex
      var row_start = 1
      for {
        dayReport <- report.dailyReports
      } {
        for {
          mt_i <- 0 to dayReport.typeList.length - 1
          dataI <- dayReport.typeList(mt_i).dataList.zipWithIndex
        } {
          val (data, idx) = dataI
          if (data._2.isDefined && data._3.isDefined && MonitorStatus.isNormal(data._3.get)) {
            if (sheet.getRow(row_start + idx) == null)
              sheet.createRow(row_start + idx).createCell(mt_i + 1).setCellValue(data._2.get)
            else
              sheet.getRow(row_start + idx).createCell(mt_i + 1).setCellValue(data._2.get)
          }

          if (mt_i == 0) {
            if (sheet.getRow(row_start + idx) == null)
              sheet.createRow(row_start + idx).createCell(0).setCellValue(data._1.toDateTime().toString("YYYY-MM-dd HH:mm"))
            else
              sheet.getRow(row_start + idx).createCell(0).setCellValue(data._1.toDateTime().toString("YYYY-MM-dd HH:mm"))
          }

          val idxOpt = graph_idx.find(p => p._1 == dayReport.typeList(mt_i).monitorType)
          if (idxOpt.isDefined) {
            val graph_idx = idxOpt.get
            val std_internal = Monitor.map(monitor).getStdInternal(graph_idx._1)
            if (std_internal.isDefined) {
              sheet.getRow(row_start + idx).createCell(22 + graph_idx._2).setCellValue(std_internal.get)
            }
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
    fillGraphHourSheet(monthlyHourReport, 2 + monthlyHourReport.dailyReports(0).typeList.length)

    wb.setActiveSheet(0)
    finishExcel(reportFilePath, pkg, wb)
  }

  def createYearlyReport(monitor: Monitor.Value, reportDate: DateTime, report: IntervalReport) = {
    implicit val (reportFilePath, pkg, wb) = prepareTemplate("yearly_report.xlsx")
    val sheet = wb.getSheetAt(0)
    sheet.getRow(2).getCell(0).setCellValue("監測站:" + Monitor.map(monitor).name)
    sheet.getRow(1).getCell(30).setCellValue("查詢日期:" + DateTime.now.toString("YYYY/MM/dd"))
    sheet.getRow(2).getCell(30).setCellValue("資料日期:" + reportDate.toString("YYYY年"))

    val abnormalColor =
      {
        val seqColors =
          for (col <- 3 to 3)
            yield sheet.getRow(2).getCell(col).getCellStyle.getFillForegroundXSSFColor
        seqColors.toArray
      }

    for {
      col <- 1 to report.typeArray.length
      mt = report.typeArray(col - 1).monitorType
      normalStyle = createStyle(mt)
      abnormalStyles = createColorStyle(abnormalColor, mt)
    } {
      if (!Monitor.map(monitor).monitorTypes.contains(mt)) {
        sheet.setColumnHidden(col, true)
      }

      for {
        row <- 4 to 4 + 12 - 1
        data = report.typeArray(col - 1).dataList(row - 4)
      } {
        val cell = sheet.getRow(row).getCell(col)
        if (data.count != 0) {
          cell.setCellValue(data.avg.get)
          if (data.effectPercent.isDefined && data.effectPercent.get >= 0.75)
            cell.setCellStyle(normalStyle)
          else
            cell.setCellStyle(abnormalStyles(0))
        } else
          sheet.getRow(row).getCell(col).setCellValue("-")
      }
      val stat = report.typeArray(col - 1).stat
      if (stat.count != 0) {
        sheet.getRow(16).getCell(col).setCellValue(stat.avg.get)
        sheet.getRow(16).getCell(col).setCellStyle(normalStyle)
        sheet.getRow(17).getCell(col).setCellValue(stat.max.get)
        sheet.getRow(17).getCell(col).setCellStyle(normalStyle)
        sheet.getRow(18).getCell(col).setCellValue(stat.min.get)
        sheet.getRow(18).getCell(col).setCellStyle(normalStyle)
        sheet.getRow(19).getCell(col).setCellValue(stat.effectPercent.get)
        sheet.getRow(19).getCell(col).setCellStyle(normalStyle)
      } else {
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

    for {
      row <- 4 to 4 + 12 - 1
      mt <- MonitorType.monitorReportList.zipWithIndex
      data = rateList(row - 4)
      cell = sheet.getRow(row).getCell(mt._2 + 1)
    } {
      cell.setCellValue(data.rateMap(mt._1) * 100)
    }

    for {
      mt <- MonitorType.monitorReportList.zipWithIndex
      stat = statMap(mt._1)
    } {
      sheet.getRow(16).getCell(mt._2 + 1).setCellValue(stat.min.get * 100)
      sheet.getRow(17).getCell(mt._2 + 1).setCellValue(stat.max.get * 100)
      sheet.getRow(18).getCell(mt._2 + 1).setCellValue(stat.avg.get * 100)
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
      cell = sheet.getRow(row).getCell(m._2 + 1)
    } {
      cell.setCellValue(data * 100)
    }

    for {
      m <- Monitor.mvList.zipWithIndex
      stat = statMap(m._1)
    } {
      if(stat.count >0){
        sheet.getRow(16).getCell(m._2 + 1).setCellValue(stat.min.get * 100)
        sheet.getRow(17).getCell(m._2 + 1).setCellValue(stat.max.get * 100)
        sheet.getRow(18).getCell(m._2 + 1).setCellValue(stat.avg.get * 100)
      }else{
        sheet.getRow(16).getCell(m._2 + 1).setCellValue("-")
        sheet.getRow(17).getCell(m._2 + 1).setCellValue("-")
        sheet.getRow(18).getCell(m._2 + 1).setCellValue("-")
      }
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
        if (psi._1.isDefined)
          sheet.getRow(row).getCell(2 + mtv._2 * 2 + 1).setCellValue(psi._1.get)
        else
          sheet.getRow(row).getCell(2 + mtv._2 * 2 + 1).setCellValue("-")

        if (psi._2.isDefined)
          sheet.getRow(row).getCell(2 + mtv._2 * 2).setCellValue(psi._2.get)
        else
          sheet.getRow(row).getCell(2 + mtv._2 * 2).setCellValue("-")
      }
    }

    finishExcel(reportFilePath, pkg, wb)
  }

  import models.Realtime._
  def psiMonthlyReport(monitor: Monitor.Value, reportDate: DateTime, psiDailyList: List[PsiReport], nDays: Int) = {
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
        if (psi._1.isDefined)
          sheet.getRow(row).getCell(2 + mtv._2 * 2 + 1).setCellValue(psi._1.get)
        else
          sheet.getRow(row).getCell(2 + mtv._2 * 2 + 1).setCellValue("-")

        if (psi._2.isDefined)
          sheet.getRow(row).getCell(2 + mtv._2 * 2).setCellValue(psi._2.get)
        else
          sheet.getRow(row).getCell(2 + mtv._2 * 2).setCellValue("-")
      }
    }

    finishExcel(reportFilePath, pkg, wb)
  }

  def covertDegToDir(degree: Float) = {
    val dirMap =
      Map(
        (0 -> "北"), (1 -> "北北東"), (2 -> "東北"), (3 -> "東北東"), (4 -> "東"),
        (5 -> "東南東"), (6 -> "東南"), (7 -> "南南東"), (8 -> "南"),
        (9 -> "南南西"), (10 -> "西南"), (11 -> "西西南"), (12 -> "西"),
        (13 -> "西北西"), (14 -> "西北"), (15 -> "北北西"))

    val step = 360 / 16
    val dir = Math.ceil((degree - (step / 2)) / step).toInt % 16
    dirMap(dir)
  }

  def epaCompareReport(monitor: Monitor.Value, epaMonitor: EpaMonitor.Value, reportDate: DateTime, myMap: Map[MonitorType.Value, (Map[DateTime, (Option[Float], Option[String])], Stat)], epaMap: Map[MonitorType.Value, (Map[DateTime, EpaHourRecord], Stat)], hours: List[DateTime]) = {
    implicit val (reportFilePath, pkg, wb) = prepareTemplate("epa_compare.xlsx")
    val evaluator = wb.getCreationHelper().createFormulaEvaluator()

    val sheet = wb.getSheetAt(0)
    sheet.getRow(2).getCell(0).setCellValue("測站:" + Monitor.map(monitor).name + "/環保署測站:" + EpaMonitor.map(epaMonitor).name)
    sheet.getRow(1).getCell(24).setCellValue("查詢日期:" + DateTime.now.toString("YYYY/MM/dd"))
    sheet.getRow(2).getCell(24).setCellValue("資料日期:" + reportDate.toString("YYYY年MM月dd"))

    val abnormalColor =
      {
        val seqColors =
          for (col <- 9 to 13)
            yield wb.getSheetAt(0).getRow(2).getCell(col).getCellStyle.getFillForegroundXSSFColor

        seqColors.toArray
      }

    var row = 5
    for {
      mt <- MonitorType.epaReportList.zipWithIndex
      normalStyle = createStyle(mt._1)
      abnormalStyles = createColorStyle(abnormalColor, mt._1)
      //row = mt._2 * 2 + 5
    } {
      sheet.getRow(row).getCell(1).setCellValue(Monitor.map(monitor).name)
      sheet.getRow(row + 1).getCell(1).setCellValue(EpaMonitor.map(epaMonitor).name)
      if (mt._1 == MonitorType.C212) {
        sheet.getRow(row + 2).getCell(1).setCellValue(Monitor.map(monitor).name)
        sheet.getRow(row + 3).getCell(1).setCellValue(EpaMonitor.map(epaMonitor).name)
      }

      for {
        hr <- hours.zipWithIndex
        col = hr._2 + 2
        cell = sheet.getRow(row).getCell(col)
      } {
        val vOpt = myMap(mt._1)._1.get(hr._1)
        if (vOpt.isDefined) {
          val p = vOpt.get
          if (mt._1 == MonitorType.C212) {
            val cellDir = sheet.getRow(row + 2).getCell(col)
            cell.setCellValue(p._1.get)
            cellDir.setCellValue(covertDegToDir(p._1.get))
          } else
            cell.setCellValue(p._1.get)

          val status = p._2.get
          val cellStyle = getStyle(status, normalStyle, abnormalStyles)
          cell.setCellStyle(cellStyle)
        } else {
          cell.setCellValue("-")
        }
      }

      if (myMap(mt._1)._2.count != 0) {
        val stat = myMap(mt._1)._2
        sheet.getRow(row).getCell(26).setCellValue(stat.min.get)
        sheet.getRow(row + 2).getCell(26).setCellValue(covertDegToDir(stat.min.get))
        sheet.getRow(row).getCell(27).setCellValue(stat.max.get)
        sheet.getRow(row + 2).getCell(27).setCellValue(covertDegToDir(stat.max.get))
        sheet.getRow(row).getCell(28).setCellValue(stat.avg.get)
        sheet.getRow(row + 2).getCell(28).setCellValue(covertDegToDir(stat.avg.get))
      } else {
        sheet.getRow(row).getCell(26).setCellValue("-")
        sheet.getRow(row).getCell(27).setCellValue("-")
        sheet.getRow(row).getCell(28).setCellValue("-")
        sheet.getRow(row + 2).getCell(26).setCellValue("-")
        sheet.getRow(row + 2).getCell(27).setCellValue("-")
        sheet.getRow(row + 2).getCell(28).setCellValue("-")
      }

      for {
        hr <- hours.zipWithIndex
        col = hr._2 + 2
        cell = sheet.getRow(row + 1).getCell(col)
      } {
        val vOpt = epaMap(mt._1)._1.get(hr._1)
        if (vOpt.isEmpty) {
          cell.setCellValue("-")
        } else {
          val epaRecord = vOpt.get
          if (mt._1 == MonitorType.C212) {
            val cellDir = sheet.getRow(row + 3).getCell(col)
            cell.setCellValue(epaRecord.value)
            cellDir.setCellValue(covertDegToDir(epaRecord.value))
          } else
            cell.setCellValue(epaRecord.value)

          cell.setCellStyle(normalStyle)
        }
      }

      if (epaMap(mt._1)._2.count != 0) {
        val stat = epaMap(mt._1)._2
        sheet.getRow(row + 1).getCell(26).setCellValue(stat.min.get)
        sheet.getRow(row + 1).getCell(27).setCellValue(stat.max.get)
        sheet.getRow(row + 1).getCell(28).setCellValue(stat.avg.get)
        sheet.getRow(row + 3).getCell(26).setCellValue(covertDegToDir(stat.min.get))
        sheet.getRow(row + 3).getCell(27).setCellValue(covertDegToDir(stat.max.get))
        sheet.getRow(row + 3).getCell(28).setCellValue(covertDegToDir(stat.avg.get))
      } else {
        sheet.getRow(row + 1).getCell(26).setCellValue("-")
        sheet.getRow(row + 1).getCell(27).setCellValue("-")
        sheet.getRow(row + 1).getCell(28).setCellValue("-")
        sheet.getRow(row + 3).getCell(26).setCellValue("-")
        sheet.getRow(row + 3).getCell(27).setCellValue("-")
        sheet.getRow(row + 3).getCell(28).setCellValue("-")
      }

      if (mt._1 == MonitorType.C212)
        row += 4
      else
        row += 2
    }
    finishExcel(reportFilePath, pkg, wb)
  }

  import Calibration._
  def calibrationDailyReport(title: String, reportDate: DateTime, report: List[CalibrationItem], displayDate: Boolean = false) = {
    val (reportFilePath, pkg, wb) = prepareTemplate("calibration_daily.xlsx")
    val evaluator = wb.getCreationHelper().createFormulaEvaluator()

    val sheet = wb.getSheetAt(0)
    sheet.getRow(0).getCell(0).setCellValue(title)
    sheet.getRow(1).getCell(11).setCellValue("查詢日期:" + DateTime.now.toString("YYYY/MM/dd"))
    sheet.getRow(2).getCell(11).setCellValue("資料日期:" + reportDate.toString("YYYY/MM/dd"))

    val internalStyle = wb.getSheetAt(0).getRow(2).getCell(9).getCellStyle
    val lawStyle = wb.getSheetAt(0).getRow(2).getCell(10).getCellStyle

    val styles =
      for (row <- 0 to 1) yield {
        for (col <- 0 to 2)
          yield wb.getSheetAt(1).getRow(row).getCell(col).getCellStyle
      }

    var styleIdx = -1
    var currentMonitor: Option[Monitor.Value] = None
    var offset = -1
    def adjustStyleIdx(m: Monitor.Value, row: Int) = {
      if (currentMonitor != Some(m)) {
        currentMonitor = Some(m)
        styleIdx += 1
        styleIdx %= 2
        if (offset < 0) {
          offset = 0
        } else {
          copyHeaderRowTo(row + offset)
          offset += 1
        }
      }
    }

    def copyHeaderRowTo(row: Int) {
      val headerRow = sheet.getRow(3)
      val newRow = sheet.createRow(row)
      for (col <- 0 to 11) {
        val headerCell = headerRow.getCell(col)
        val cell = newRow.createCell(col)
        cell.setCellStyle(headerCell.getCellStyle)
        cell.setCellValue(headerCell.getStringCellValue)
      }
    }
    def fillCell(cell: XSSFCell, v: String, idx: Int) {
      cell.setCellValue(v)
      cell.setCellStyle(styles(styleIdx)(idx))
    }

    def fillCellF(cell: XSSFCell, v: Float, idx: Int) {
      cell.setCellValue(v)
      cell.setCellStyle(styles(styleIdx)(idx))
    }

    for {
      row <- 4 to (4 + report.length - 1)
      item = report(row - 4)
      mt = item.monitorType
    } {
      adjustStyleIdx(item.monitor, row)
      val newRow = sheet.createRow(row + offset)
      fillCell(newRow.createCell(0), Monitor.map(item.monitor).name, 0)
      if (!displayDate)
        fillCell(newRow.createCell(1), item.startTime.toString("HH:mm"), 0)
      else
        fillCell(newRow.createCell(1), item.startTime.toString("YYYY-M-d HH:mm"), 0)

      fillCell(newRow.createCell(2), MonitorType.map(item.monitorType).desp, 0)
      if (MonitorType.map(item.monitorType).zd_law.isDefined && item.z_val > MonitorType.map(item.monitorType).zd_law.get) {
        fillCellF(newRow.createCell(3), item.z_val, 2)
      } else if (MonitorType.map(item.monitorType).zd_internal.isDefined && item.z_val > MonitorType.map(item.monitorType).zd_internal.get) {
        fillCellF(newRow.createCell(3), item.z_val, 1)
      } else
        fillCellF(newRow.createCell(3), item.z_val, 0)

      if (MonitorType.map(item.monitorType).zd_internal.isDefined)
        fillCellF(newRow.createCell(4), MonitorType.map(item.monitorType).zd_internal.get, 0)

      if (MonitorType.map(item.monitorType).zd_law.isDefined)
        fillCellF(newRow.createCell(5), MonitorType.map(item.monitorType).zd_law.get, 0)

      fillCellF(newRow.createCell(6), item.s_std, 0)
      fillCellF(newRow.createCell(7), item.s_sval, 0)
      if (MonitorType.map(item.monitorType).sd_law.isDefined && item.sd_pnt > MonitorType.map(item.monitorType).sd_law.get) {
        fillCellF(newRow.createCell(8), item.sd_pnt, 2)
      } else if (MonitorType.map(item.monitorType).sd_internal.isDefined && item.sd_pnt > MonitorType.map(item.monitorType).sd_internal.get) {
        fillCellF(newRow.createCell(8), item.sd_pnt, 1)
      } else
        fillCellF(newRow.createCell(8), item.sd_pnt, 0)

      if (MonitorType.map(item.monitorType).sd_internal.isDefined)
        fillCellF(newRow.createCell(9), MonitorType.map(item.monitorType).sd_internal.get, 0)

      if (MonitorType.map(item.monitorType).sd_law.isDefined)
        fillCellF(newRow.createCell(10), MonitorType.map(item.monitorType).sd_law.get, 0)

      if (MonitorType.map(item.monitorType).zd_law.isDefined && MonitorType.map(item.monitorType).sd_law.isDefined) {
        if (item.z_val > MonitorType.map(item.monitorType).zd_law.get || item.sd_pnt > MonitorType.map(item.monitorType).sd_law.get) {
          fillCell(newRow.createCell(11), "失敗", 2)
        } else {
          if (item.z_val > MonitorType.map(item.monitorType).zd_internal.get || item.sd_pnt > MonitorType.map(item.monitorType).sd_internal.get) {
            fillCell(newRow.createCell(11), "成功", 1)
          } else
            fillCell(newRow.createCell(11), "成功", 0)
        }
      }
    }
    finishExcel(reportFilePath, pkg, wb)
  }

  def calibrationMonthlyAllReport(monitor: Monitor.Value, reportDate: DateTime, map: Map[MonitorType.Value, Map[String, Calibration.CalibrationItem]], nDays: Int) = {
    val (reportFilePath, pkg, wb) = prepareTemplate("all_calibration_monthly.xlsx")
    val evaluator = wb.getCreationHelper().createFormulaEvaluator()

    def fillMonitorTypeReport(monitorType: MonitorType.Value, map: Map[String, Calibration.CalibrationItem], sheetIdx: Int) = {
      val sheet = wb.getSheetAt(sheetIdx)
      sheet.getRow(1).getCell(11).setCellValue("查詢日期:" + DateTime.now.toString("YYYY/MM/dd"))
      sheet.getRow(2).getCell(11).setCellValue("資料日期:" + reportDate.toString("YYYY/MM"))
      sheet.getRow(2).getCell(0).setCellValue("測站:" + Monitor.map(monitor).name)

      val internalStyle = wb.getSheetAt(0).getRow(2).getCell(8).getCellStyle
      val lawStyle = wb.getSheetAt(0).getRow(2).getCell(8).getCellStyle

      for {
        row <- 4 to (4 + nDays - 1)
        itemOpt = map.get((row - 3).toString)
      } {
        if (itemOpt.isDefined) {
          val item = itemOpt.get
          sheet.getRow(row).getCell(1).setCellValue(item.startTime.toString("HH:mm"))
          sheet.getRow(row).getCell(2).setCellValue(MonitorType.map(item.monitorType).desp)
          if (item.z_val > MonitorType.map(item.monitorType).zd_law.get) {
            sheet.getRow(row).getCell(3).setCellStyle(lawStyle)
          } else if (item.z_val > MonitorType.map(item.monitorType).zd_internal.get) {
            sheet.getRow(row).getCell(3).setCellStyle(internalStyle)
          }
          sheet.getRow(row).getCell(3).setCellValue(item.z_val)
          sheet.getRow(row).getCell(4).setCellValue(MonitorType.map(item.monitorType).zd_internal.get)
          sheet.getRow(row).getCell(5).setCellValue(MonitorType.map(item.monitorType).zd_law.get)
          sheet.getRow(row).getCell(6).setCellValue(item.s_std)
          sheet.getRow(row).getCell(7).setCellValue(item.s_sval)
          if (item.sd_pnt > MonitorType.map(item.monitorType).sd_law.get) {
            sheet.getRow(row).getCell(8).setCellStyle(lawStyle)
          } else if (item.sd_pnt > MonitorType.map(item.monitorType).sd_internal.get) {
            sheet.getRow(row).getCell(8).setCellStyle(lawStyle)
          }
          sheet.getRow(row).getCell(8).setCellValue(item.sd_pnt)
          sheet.getRow(row).getCell(9).setCellValue(MonitorType.map(item.monitorType).sd_internal.get)
          sheet.getRow(row).getCell(10).setCellValue(MonitorType.map(item.monitorType).sd_law.get)
          if (item.z_val > MonitorType.map(item.monitorType).zd_law.get || item.sd_pnt > MonitorType.map(item.monitorType).sd_law.get) {
            sheet.getRow(row).getCell(11).setCellStyle(lawStyle)
            sheet.getRow(row).getCell(11).setCellValue("失敗")
          } else {
            if (item.z_val > MonitorType.map(item.monitorType).zd_internal.get || item.sd_pnt > MonitorType.map(item.monitorType).sd_internal.get) {
              sheet.getRow(row).getCell(11).setCellStyle(internalStyle)
            }
            sheet.getRow(row).getCell(11).setCellValue("成功")
          }

        }
      }
      sheet.getRow(36).getCell(0).setCellValue(s"${Monitor.map(monitor).name} (${MonitorType.map(monitorType).desp})零點校正趨勢圖")
      sheet.getRow(37).getCell(0).setCellValue(s"${Monitor.map(monitor).name} (${MonitorType.map(monitorType).desp})全幅校正趨勢圖")
      //sheet.getRow(38).getCell(0).setCellValue(s"${Monitor.map(monitor).name} (${MonitorType.map(monitorType).desp})全幅讀值趨勢圖")
    }

    var first_mtidx = -1
    for ((mt, idx) <- MonitorType.calibrationList.zipWithIndex) {
      val mtMap = map.get(mt)
      if (mtMap.isDefined && mtMap.get.size > 0) {
        fillMonitorTypeReport(mt, mtMap.get, idx)
        if (first_mtidx == -1) {
          first_mtidx = idx
        }
      } else {
        wb.setSheetHidden(idx, true)
      }
    }
    if (first_mtidx != -1)
      wb.setActiveSheet(first_mtidx)

    finishExcel(reportFilePath, pkg, wb)
  }

  def calibrationMonthlyReport(monitor: Monitor.Value, monitorType: MonitorType.Value, reportDate: DateTime, map: Map[String, Calibration.CalibrationItem], nDays: Int) = {
    val (reportFilePath, pkg, wb) = prepareTemplate("calibration_monthly.xlsx")
    val evaluator = wb.getCreationHelper().createFormulaEvaluator()

    val sheet = wb.getSheetAt(0)
    sheet.getRow(1).getCell(11).setCellValue("查詢日期:" + DateTime.now.toString("YYYY/MM/dd"))
    sheet.getRow(2).getCell(11).setCellValue("資料日期:" + reportDate.toString("YYYY/MM"))
    sheet.getRow(2).getCell(0).setCellValue("測站:" + Monitor.map(monitor).name)

    val internalStyle = wb.getSheetAt(0).getRow(2).getCell(8).getCellStyle
    val lawStyle = wb.getSheetAt(0).getRow(2).getCell(8).getCellStyle

    for {
      row <- 4 to (4 + nDays - 1)
      itemOpt = map.get((row - 4).toString)
    } {
      if (itemOpt.isDefined) {
        val item = itemOpt.get
        sheet.getRow(row).getCell(1).setCellValue(item.startTime.toString("HH:mm"))
        sheet.getRow(row).getCell(2).setCellValue(MonitorType.map(item.monitorType).desp)
        if (item.z_val > MonitorType.map(item.monitorType).zd_law.get) {
          sheet.getRow(row).getCell(3).setCellStyle(lawStyle)
        } else if (item.z_val > MonitorType.map(item.monitorType).zd_internal.get) {
          sheet.getRow(row).getCell(3).setCellStyle(internalStyle)
        }
        sheet.getRow(row).getCell(3).setCellValue(item.z_val)
        sheet.getRow(row).getCell(4).setCellValue(MonitorType.map(item.monitorType).zd_internal.get)
        sheet.getRow(row).getCell(5).setCellValue(MonitorType.map(item.monitorType).zd_law.get)
        sheet.getRow(row).getCell(6).setCellValue(item.s_std)
        sheet.getRow(row).getCell(7).setCellValue(item.s_sval)
        if (item.sd_pnt > MonitorType.map(item.monitorType).sd_law.get) {
          sheet.getRow(row).getCell(8).setCellStyle(lawStyle)
        } else if (item.sd_pnt > MonitorType.map(item.monitorType).sd_internal.get) {
          sheet.getRow(row).getCell(8).setCellStyle(lawStyle)
        }
        sheet.getRow(row).getCell(8).setCellValue(item.sd_pnt)
        sheet.getRow(row).getCell(9).setCellValue(MonitorType.map(item.monitorType).sd_internal.get)
        sheet.getRow(row).getCell(10).setCellValue(MonitorType.map(item.monitorType).sd_law.get)
        if (item.z_val > MonitorType.map(item.monitorType).zd_law.get || item.sd_pnt > MonitorType.map(item.monitorType).sd_law.get) {
          sheet.getRow(row).getCell(11).setCellStyle(lawStyle)
          sheet.getRow(row).getCell(11).setCellValue("失敗")
        } else {
          if (item.z_val > MonitorType.map(item.monitorType).zd_internal.get || item.sd_pnt > MonitorType.map(item.monitorType).sd_internal.get) {
            sheet.getRow(row).getCell(11).setCellStyle(internalStyle)
          }
          sheet.getRow(row).getCell(11).setCellValue("成功")
        }

      }
    }
    sheet.getRow(36).getCell(0).setCellValue(s"${Monitor.map(monitor).name} (${MonitorType.map(monitorType).desp})零點校正趨勢圖")
    sheet.getRow(37).getCell(0).setCellValue(s"${Monitor.map(monitor).name} (${MonitorType.map(monitorType).desp})全幅校正趨勢圖")

    finishExcel(reportFilePath, pkg, wb)
  }

  import controllers.Realtime.HighchartData
  def exportChartData(chart: HighchartData, monitorTypes: Array[MonitorType.Value]): File = {
    val precArray = monitorTypes.map { mt => MonitorType.map(mt).prec }
    exportChartData(chart, precArray)
  }

  def exportChartData(chart: HighchartData, precArray: Array[Int]) = {
    val (reportFilePath, pkg, wb) = prepareTemplate("chart_export.xlsx")
    val evaluator = wb.getCreationHelper().createFormulaEvaluator()
    val format = wb.createDataFormat();

    val sheet = wb.getSheetAt(0)
    val headerRow = sheet.createRow(0)
    headerRow.createCell(0).setCellValue("時間")

    var pos = 0
    for {
      col <- 1 to chart.series.length
      series = chart.series(col - 1)
    } {
      headerRow.createCell(pos + 1).setCellValue(series.name)
      pos += 1
      if (series.status.isDefined) {
        headerRow.createCell(pos + 1).setCellValue("狀態碼")
        pos += 1
      }
    }

    val styles = precArray.map { prec =>
      val format_str = "0." + "0" * prec
      val style = wb.createCellStyle();
      style.setDataFormat(format.getFormat(format_str))
      style
    }

    // Categories data
    if (chart.xAxis.categories.isDefined) {
      val timeList = chart.xAxis.categories.get
      for (row <- timeList.zipWithIndex) {
        val rowNo = row._2 + 1
        val thisRow = sheet.createRow(rowNo)
        thisRow.createCell(0).setCellValue(row._1)

        for {
          col <- 1 to chart.series.length
          series = chart.series(col - 1)
        } {
          val cell = thisRow.createCell(col)
          cell.setCellStyle(styles(col - 1))

          val pair = series.data(rowNo - 1)
          if (pair.length == 2 && pair(1).isDefined) {
            cell.setCellValue(pair(1).get)
          }
          //val pOpt = series.data(rowNo-1)
          //if(pOpt.isDefined){
          //  cell.setCellValue(pOpt.get)
          //}

        }
      }
    } else {
      val rowMax = chart.series.map(s => s.data.length).max
      for (row <- 1 to rowMax) {
        val thisRow = sheet.createRow(row)
        val timeCell = thisRow.createCell(0)
        pos = 0
        for {
          col <- 1 to chart.series.length
          series = chart.series(col - 1)
        } {
          val cell = thisRow.createCell(pos + 1)
          pos += 1
          cell.setCellStyle(styles(col - 1))

          val pair = series.data(row - 1)
          if (col == 1) {
            val dt = new DateTime(pair(0).get.toLong)
            timeCell.setCellValue(dt.toString("YYYY/MM/dd HH:mm"))
          }
          if (pair(1).isDefined) {
            cell.setCellValue(pair(1).get)
          }

          if (series.status.isDefined) {
            val statusCell = thisRow.createCell(pos + 1)
            pos += 1
            val statusOpt = series.status.get(row - 1)
            if (statusOpt.isDefined) {
              statusCell.setCellValue(statusOpt.get)
            }
          }
        }
      }
    }

    finishExcel(reportFilePath, pkg, wb)
  }

  def exportWeekForm(ticket: Ticket, usrMap: Map[Int, User]) = {
    val (reportFilePath, pkg, wb) = prepareTemplate("weekMaintance.xlsx")
    val evaluator = wb.getCreationHelper().createFormulaEvaluator()
    val format = wb.createDataFormat();

    val form = ticket.getForm

    val sheet = wb.getSheetAt(0)
    val monitorName = Monitor.map(ticket.monitor).name
    sheet.getRow(1).getCell(1).setCellValue(monitorName)
    sheet.getRow(42).getCell(1).setCellValue(monitorName)
    sheet.getRow(75).getCell(1).setCellValue(monitorName)
    sheet.getRow(113).getCell(1).setCellValue(monitorName)

    val dateStr = ticket.executeDate.toString("YY/MM/d")
    sheet.getRow(1).getCell(5).setCellValue(dateStr)
    sheet.getRow(42).getCell(5).setCellValue(dateStr)
    sheet.getRow(75).getCell(5).setCellValue(dateStr)
    sheet.getRow(113).getCell(5).setCellValue(dateStr)

    val usrName = usrMap(ticket.owner_id).name
    sheet.getRow(2).getCell(5).setCellValue(usrName)
    sheet.getRow(43).getCell(5).setCellValue(usrName)
    sheet.getRow(76).getCell(5).setCellValue(usrName)
    sheet.getRow(114).getCell(5).setCellValue(usrName)

    sheet.getRow(3).getCell(4).setCellValue(form.getBoolSeq("☑", "□"))
    sheet.getRow(4).getCell(4).setCellValue(form.getBoolSeq("☑", "□"))
    sheet.getRow(6).getCell(4).setCellValue(form.getBoolSeq("☑", "□"))
    sheet.getRow(7).getCell(4).setCellValue(form.getBoolSeq("☑", "□"))
    sheet.getRow(8).getCell(2).setCellValue(form.getStrSeq)
    sheet.getRow(8).getCell(4).setCellValue(form.getBoolSeq("☑", "□"))

    sheet.getRow(10).getCell(4).setCellValue(form.getBoolSeq("☑", "□"))
    for (row <- 12 to 22) {
      sheet.getRow(row).getCell(2).setCellValue(form.getStrSeq)
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("☑", "□"))
    }
    sheet.getRow(23).getCell(4).setCellValue(form.getBoolSeq("☑", "□"))
    sheet.getRow(25).getCell(4).setCellValue(form.getBoolSeq("☑", "□"))
    for (row <- 27 to 36) {
      sheet.getRow(row).getCell(2).setCellValue(form.getStrSeq)
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("☑", "□"))
    }
    sheet.getRow(37).getCell(4).setCellValue(form.getBoolSeq("☑", "□"))

    //Page 2
    sheet.getRow(44).getCell(4).setCellValue(form.getBoolSeq("☑", "□"))
    for (row <- 46 to 51) {
      sheet.getRow(row).getCell(2).setCellValue(form.getStrSeq)
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("☑", "□"))
    }
    sheet.getRow(52).getCell(4).setCellValue(form.getBoolSeq("☑", "□"))
    sheet.getRow(54).getCell(4).setCellValue(form.getBoolSeq("☑", "□"))
    for (row <- 56 to 59) {
      sheet.getRow(row).getCell(2).setCellValue(form.getStrSeq)
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("☑", "□"))
    }
    sheet.getRow(60).getCell(4).setCellValue(form.getBoolSeq("☑", "□"))
    sheet.getRow(62).getCell(4).setCellValue(form.getBoolSeq("☑", "□"))
    for (row <- 64 to 67) {
      sheet.getRow(row).getCell(2).setCellValue(form.getStrSeq)
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("☑", "□"))
    }
    sheet.getRow(68).getCell(4).setCellValue(form.getBoolSeq("☑", "□"))
    sheet.getRow(69).getCell(4).setCellValue(form.getBoolSeq("☑", "□"))
    sheet.getRow(70).getCell(4).setCellValue(form.getBoolSeq("☑", "□"))
    //Page 3
    sheet.getRow(77).getCell(4).setCellValue(form.getBoolSeq("☑", "□"))
    for (row <- 78 to 78) {
      sheet.getRow(row).getCell(2).setCellValue(form.getStrSeq)
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("☑", "□"))
    }
    sheet.getRow(79).getCell(4).setCellValue(form.getBoolSeq("☑", "□"))
    sheet.getRow(80).getCell(4).setCellValue(form.getBoolSeq("☑", "□"))
    sheet.getRow(81).getCell(4).setCellValue(form.getBoolSeq("☑", "□"))
    sheet.getRow(83).getCell(4).setCellValue(form.getBoolSeq("☑", "□"))
    sheet.getRow(84).getCell(2).setCellValue("溫度：" + form.getStrSeq)
    sheet.getRow(84).getCell(3).setCellValue("濕度：" + form.getStrSeq)
    sheet.getRow(84).getCell(4).setCellValue(form.getBoolSeq("☑", "□"))
    sheet.getRow(85).getCell(4).setCellValue(form.getBoolSeq("☑", "□"))
    sheet.getRow(86).getCell(4).setCellValue(form.getBoolSeq("☑", "□"))
    sheet.getRow(89).getCell(2).setCellValue(form.getStrSeq)
    sheet.getRow(89).getCell(4).setCellValue(form.getBoolSeq("☑", "□"))
    sheet.getRow(90).getCell(4).setCellValue(form.getBoolSeq("☑", "□"))
    sheet.getRow(91).getCell(2).setCellValue("用電量：" + form.getStrSeq)
    sheet.getRow(91).getCell(4).setCellValue(form.getBoolSeq("☑", "□"))
    for (row <- 92 to 96) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("☑", "□"))
    }
    sheet.getRow(98).getCell(4).setCellValue(form.getBoolSeq("☑", "□"))
    for (row <- 104 to 105) {
      sheet.getRow(row).getCell(2).setCellValue(form.getStrSeq)
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("☑", "□"))
    }
    for (row <- 106 to 108) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("☑", "□"))
    }

    //Page 4
    for (row <- 116 to 119) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("☑", "□"))
    }

    for (row <- 121 to 126) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("☑", "□"))
    }
    for (row <- 128 to 129) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("☑", "□"))
    }

    sheet.getRow(131).getCell(1).setCellValue(form.getComment(0))
    sheet.getRow(139).getCell(4).setCellValue(usrMap(ticket.owner_id).name)

    finishExcel(reportFilePath, pkg, wb)
  }

  def exportBiWeekForm(ticket: Ticket, usrMap: Map[Int, User]) = {
    val (reportFilePath, pkg, wb) = prepareTemplate("biweekForm.xlsx")
    val evaluator = wb.getCreationHelper().createFormulaEvaluator()
    val format = wb.createDataFormat();

    val form = ticket.getForm

    val sheet = wb.getSheetAt(0)
    val monitorName = Monitor.map(ticket.monitor).name
    sheet.getRow(1).getCell(1).setCellValue(monitorName)

    val dateStr = ticket.executeDate.toString("YY/MM/d")
    sheet.getRow(1).getCell(11).setCellValue(dateStr)

    val usrName = usrMap(ticket.owner_id).name
    sheet.getRow(2).getCell(11).setCellValue(usrName)
    sheet.getRow(36).getCell(8).setCellValue(usrName)

    for (row <- 4 to 10) {
      sheet.getRow(row).getCell(1).setCellValue(form.getStrSeq)
      sheet.getRow(row).getCell(3).setCellValue(form.getBoolSeq("☑", "□"))
    }

    for (row <- 4 to 10) {
      sheet.getRow(row).getCell(4).setCellValue(form.getStrSeq)
      sheet.getRow(row).getCell(5).setCellValue(form.getStrSeq)
      sheet.getRow(row).getCell(6).setCellValue(form.getStrSeq)
      sheet.getRow(row).getCell(7).setCellValue(form.getBoolSeq("☑", "□"))
      sheet.getRow(row).getCell(8).setCellValue(form.getStrSeq)
      sheet.getRow(row).getCell(9).setCellValue(form.getStrSeq)
      sheet.getRow(row).getCell(10).setCellValue(form.getStrSeq)
      sheet.getRow(row).getCell(11).setCellValue(form.getStrSeq)
    }

    for (row <- 12 to 18) {
      sheet.getRow(row).getCell(7).setCellValue(form.getBoolSeq("☑", "□"))
    }
    sheet.getRow(20).getCell(1).setCellValue(form.getComment(0))

    finishExcel(reportFilePath, pkg, wb)
  }

  def exportMonthForm(ticket: Ticket, usrMap: Map[Int, User]) = {
    val (reportFilePath, pkg, wb) = prepareTemplate("monthForm.xlsx")
    val evaluator = wb.getCreationHelper().createFormulaEvaluator()
    val format = wb.createDataFormat();

    val form = ticket.getForm

    val sheet = wb.getSheetAt(0)
    val monitorName = Monitor.map(ticket.monitor).name
    sheet.getRow(1).getCell(1).setCellValue(monitorName)
    sheet.getRow(35).getCell(1).setCellValue(monitorName)

    val dateStr = ticket.executeDate.toString("YY/MM/d")
    sheet.getRow(1).getCell(5).setCellValue(dateStr)
    sheet.getRow(35).getCell(5).setCellValue(dateStr)

    val usrName = usrMap(ticket.owner_id).name
    sheet.getRow(2).getCell(5).setCellValue(usrName)
    sheet.getRow(36).getCell(5).setCellValue(usrName)
    sheet.getRow(72).getCell(4).setCellValue(usrName)

    for (row <- 3 to 7) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("☑", "□"))
    }

    for (row <- 9 to 10) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("☑", "□"))
    }

    for (row <- 12 to 15) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("☑", "□"))
    }

    for (row <- 17 to 20) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("☑", "□"))
    }
    for (row <- 22 to 25) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("☑", "□"))
    }
    for (row <- 27 to 30) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("☑", "□"))
    }
    for (row <- 37 to 41) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("☑", "□"))
    }
    for (row <- 43 to 45) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("☑", "□"))
    }
    for (row <- 47 to 50) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("☑", "□"))
    }
    for (row <- 52 to 58) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("☑", "□"))
    }
    for (row <- 60 to 62) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("☑", "□"))
    }

    sheet.getRow(64).getCell(1).setCellValue(form.getComment(0))

    finishExcel(reportFilePath, pkg, wb)
  }

  def exportQuarterForm(ticket: Ticket, usrMap: Map[Int, User]) = {
    val (reportFilePath, pkg, wb) = prepareTemplate("quarterForm.xlsx")
    val evaluator = wb.getCreationHelper().createFormulaEvaluator()
    val format = wb.createDataFormat();

    val form = ticket.getForm

    val sheet = wb.getSheetAt(0)
    val monitorName = Monitor.map(ticket.monitor).name
    sheet.getRow(1).getCell(1).setCellValue(monitorName)
    sheet.getRow(40).getCell(1).setCellValue(monitorName)

    val dateStr = ticket.executeDate.toString("YY/MM/d")
    sheet.getRow(1).getCell(5).setCellValue(dateStr)
    sheet.getRow(40).getCell(5).setCellValue(dateStr)

    val usrName = usrMap(ticket.owner_id).name
    sheet.getRow(2).getCell(5).setCellValue(usrName)
    sheet.getRow(41).getCell(5).setCellValue(usrName)
    sheet.getRow(64).getCell(4).setCellValue(usrName)

    sheet.getRow(3).getCell(4).setCellValue(form.getBoolSeq("☑", "□"))

    for (row <- 5 to 7) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("☑", "□"))
    }

    for (row <- 9 to 14) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("☑", "□"))
    }

    for (row <- 16 to 19) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("☑", "□"))
    }

    for (row <- 21 to 24) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("☑", "□"))
    }
    for (row <- 26 to 30) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("☑", "□"))
    }
    for (row <- 32 to 35) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("☑", "□"))
    }
    for (row <- 42 to 46) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("☑", "□"))
    }
    for (row <- 48 to 50) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("☑", "□"))
    }
    for (row <- 52 to 54) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("☑", "□"))
    }

    sheet.getRow(56).getCell(1).setCellValue(form.getComment(0))

    finishExcel(reportFilePath, pkg, wb)
  }

  def exportHalfYearForm(ticket: Ticket, usrMap: Map[Int, User]) = {
    val (reportFilePath, pkg, wb) = prepareTemplate("halfYearForm.xlsx")
    val evaluator = wb.getCreationHelper().createFormulaEvaluator()
    val format = wb.createDataFormat();

    val form = ticket.getForm

    val sheet = wb.getSheetAt(0)
    val monitorName = Monitor.map(ticket.monitor).name
    sheet.getRow(1).getCell(1).setCellValue(monitorName)
    sheet.getRow(27).getCell(1).setCellValue(monitorName)

    val dateStr = ticket.executeDate.toString("YY/MM/d")
    sheet.getRow(1).getCell(5).setCellValue(dateStr)
    sheet.getRow(27).getCell(5).setCellValue(dateStr)

    val usrName = usrMap(ticket.owner_id).name
    sheet.getRow(2).getCell(5).setCellValue(usrName)
    sheet.getRow(28).getCell(5).setCellValue(usrName)
    sheet.getRow(55).getCell(4).setCellValue(usrName)

    sheet.getRow(3).getCell(4).setCellValue(form.getBoolSeq("☑", "□"))

    for (row <- 5 to 7) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("☑", "□"))
    }

    for (row <- 9 to 11) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("☑", "□"))
    }

    for (row <- 13 to 14) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("☑", "□"))
    }
    for (row <- 16 to 17) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("☑", "□"))
    }
    for (row <- 19 to 22) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("☑", "□"))
    }

    for (row <- 30 to 33) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("☑", "□"))
    }
    for (row <- 35 to 37) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("☑", "□"))
    }
    for (row <- 39 to 45) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("☑", "□"))
    }

    sheet.getRow(47).getCell(1).setCellValue(form.getComment(0))

    finishExcel(reportFilePath, pkg, wb)
  }

  def exportYearForm(ticket: Ticket, usrMap: Map[Int, User]) = {
    val (reportFilePath, pkg, wb) = prepareTemplate("YearForm.xlsx")
    val evaluator = wb.getCreationHelper().createFormulaEvaluator()
    val format = wb.createDataFormat();

    val form = ticket.getForm

    val sheet = wb.getSheetAt(0)
    val monitorName = Monitor.map(ticket.monitor).name
    sheet.getRow(1).getCell(1).setCellValue(monitorName)

    val dateStr = ticket.executeDate.toString("YY/MM/d")
    sheet.getRow(1).getCell(5).setCellValue(dateStr)

    val usrName = usrMap(ticket.owner_id).name
    sheet.getRow(2).getCell(5).setCellValue(usrName)
    sheet.getRow(28).getCell(5).setCellValue(usrName)
    sheet.getRow(42).getCell(4).setCellValue(usrName)

    sheet.getRow(3).getCell(4).setCellValue(form.getBoolSeq("☑", "□"))
    sheet.getRow(5).getCell(4).setCellValue(form.getBoolSeq("☑", "□"))

    for (row <- 7 to 9) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("☑", "□"))
    }

    for (row <- 11 to 13) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("☑", "□"))
    }

    for (row <- 15 to 16) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("☑", "□"))
    }
    for (row <- 18 to 20) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("☑", "□"))
    }
    for (row <- 22 to 24) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("☑", "□"))
    }

    for (row <- 26 to 28) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("☑", "□"))
    }
    for (row <- 30 to 32) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("☑", "□"))
    }

    sheet.getRow(34).getCell(1).setCellValue(form.getComment(0))

    finishExcel(reportFilePath, pkg, wb)
  }

  def exportRepairForm(ticket: Ticket, usrMap: Map[Int, User]) = {
    val (reportFilePath, pkg, wb) = prepareTemplate("repairForm.xlsx")
    val evaluator = wb.getCreationHelper().createFormulaEvaluator()
    val format = wb.createDataFormat();

    val form = ticket.getRepairForm

    val sheet = wb.getSheetAt(0)
    val monitorName = Monitor.map(ticket.monitor).name
    sheet.getRow(1).getCell(1).setCellValue(monitorName)

    val dateStr = ticket.executeDate.toString("YYYY/MM/dd")
    sheet.getRow(1).getCell(8).setCellValue(dateStr)

    val usrName = usrMap(ticket.owner_id).name
    sheet.getRow(1).getCell(10).setCellValue(usrName)
    sheet.getRow(37).getCell(1).setCellValue(usrName)

    sheet.getRow(5).getCell(5).setCellValue(ticket.submit_date.toString("YYYY/MM/dd"))
    sheet.getRow(6).getCell(5).setCellValue(form.start)
    sheet.getRow(5).getCell(7).setCellValue(form.end)

    val equipOpt = Equipment.getEquipment(form.equipmentId)
    if (equipOpt.isDefined) {
      val equip = equipOpt.get
      sheet.getRow(1).getCell(4).setCellValue(s"儀器設備：${equip.name}(${equip.id})")
      sheet.getRow(2).getCell(5).setCellValue(equip.brand)
      sheet.getRow(3).getCell(5).setCellValue(equip.name)
      sheet.getRow(4).getCell(5).setCellValue(equip.model)
    }
    var partRow = 24
    for (p <- form.parts) {
      val partOpt = Part.getPart(p.id)
      if (partOpt.isDefined) {
        val part = partOpt.get
        sheet.getRow(partRow).getCell(0).setCellValue(part.equipment)
        sheet.getRow(partRow).getCell(1).setCellValue(part.name)
        sheet.getRow(partRow).getCell(4).setCellValue(part.id)
        sheet.getRow(partRow).getCell(5).setCellValue(p.source)
        sheet.getRow(partRow).getCell(6).setCellValue(
          if (p.charged)
            "Yes"
          else
            "No")
        sheet.getRow(partRow).getCell(8).setCellValue(p.unit_price)
        sheet.getRow(partRow).getCell(9).setCellValue(p.amount)
        sheet.getRow(partRow).getCell(10).setCellValue(p.total)
        partRow += 1
      }
    }
    sheet.getRow(8).getCell(0).setCellValue(form.explain)
    sheet.getRow(8).getCell(5).setCellValue(form.result)

    sheet.getRow(6).getCell(1).setCellValue(form.getStr(0))
    sheet.getRow(4).getCell(10).setCellValue(form.getBoolStr(2, "☑", "□"))
    sheet.getRow(4).getCell(9).setCellValue(form.getStr(1))
    sheet.getRow(5).getCell(10).setCellValue(form.getBoolStr(1, "☑", "□"))
    sheet.getRow(6).getCell(10).setCellValue(form.getBoolStr(0, "☑", "□"))
    sheet.getRow(20).getCell(6).setCellValue(form.getBoolStr(3, "☑", "□") + "已修好")
    sheet.getRow(20).getCell(7).setCellValue(form.getBoolStr(4, "☑", "□") + "未修好")
    sheet.getRow(20).getCell(8).setCellValue(form.getBoolStr(5, "☑", "□") + "待料")

    finishExcel(reportFilePath, pkg, wb)
  }

  def monitorAbnormalReport(date: DateTime, report: Seq[AbnormalEntry]) = {
    val (reportFilePath, pkg, wb) = prepareTemplate("abnormalReport.xlsx")
    val evaluator = wb.getCreationHelper().createFormulaEvaluator()

    val sheet = wb.getSheetAt(0)
    sheet.getRow(1).getCell(4).setCellValue("印表日期:" + DateTime.now.toString("YYYY年MM月dd日"))
    sheet.getRow(2).getCell(4).setCellValue("資料日期:" + date.toString("YYYY年MM月dd日"))

    val style = wb.createCellStyle();
    val format = wb.createDataFormat();
    // Create a new font and alter it.
    val font = wb.createFont();
    font.setFontHeightInPoints(10);
    font.setFontName("標楷體");

    style.setFont(font)

    for (r <- report.reverse) {
      sheet.shiftRows(4, sheet.getLastRowNum, 1)
      val row = sheet.createRow(4)
      def fillCell(i: Int, v: String) = {
        row.createCell(i).setCellStyle(style)
        row.getCell(i).setCellValue(v)
      }

      fillCell(0, Monitor.map(r.monitor).name)
      fillCell(1, MonitorType.map(r.monitorType).desp)
      fillCell(2, date.toString("MM/dd"))
      fillCell(3, r.invalidHours)
      fillCell(4, r.explain)
    }

    finishExcel(reportFilePath, pkg, wb)
  }

  def monitorAggregateReport(date: DateTime, report: Seq[MonitorSummary]) = {
    val (reportFilePath, pkg, wb) = prepareTemplate("aggregateReport.xlsx")
    val evaluator = wb.getCreationHelper().createFormulaEvaluator()

    val sheet = wb.getSheetAt(0)
    sheet.getRow(1).getCell(3).setCellValue("印表日期:" + DateTime.now.toString("YYYY年MM月dd日"))
    sheet.getRow(2).getCell(3).setCellValue("資料日期:" + date.toString("YYYY年MM月dd日"))

    val style = wb.createCellStyle();
    val format = wb.createDataFormat();
    // Create a new font and alter it.
    val font = wb.createFont();
    font.setFontHeightInPoints(10);
    font.setFontName("標楷體");

    style.setFont(font)

    for (m <- report.zipWithIndex) {
      val r = m._1
      val idx = m._2

      def fillCell(i: Int, v: String) = {
        val row = sheet.getRow(idx + 4)
        row.getCell(i).setCellValue(v)
      }

      fillCell(0, Monitor.map(r.monitor).name)
      fillCell(1, r.desc)
      fillCell(2, r.explain)
    }

    finishExcel(reportFilePath, pkg, wb)
  }

  def monitorJournalReport(report: MonitorJournal, invalidHourList: List[(MonitorType.Value, List[MonitorInvalidHour])], userList: List[User]) = {
    val (reportFilePath, pkg, wb) = prepareTemplate("monitorJournal.xlsx")
    val evaluator = wb.getCreationHelper().createFormulaEvaluator()

    val sheet = wb.getSheetAt(0)
    sheet.getRow(1).getCell(0).setCellValue("測站名稱:" + Monitor.map(report.monitor).name)
    sheet.getRow(1).getCell(6).setCellValue("印表日期:" + DateTime.now.toString("YYYY年MM月dd日"))
    sheet.getRow(2).getCell(0).setCellValue("到站日期:" + report.date.toString("YYYY年MM月dd日"))
    if (report.operator_id.isDefined) {
      val operatorOpt = userList.find { u => u.id == report.operator_id }
      if (operatorOpt.isDefined) {
        val operator = operatorOpt.get
        sheet.getRow(3).getCell(1).setCellValue(operator.name)
      }
    }

    sheet.getRow(3).getCell(6).setCellValue(report.enter_time.toString())
    sheet.getRow(3).getCell(7).setCellValue(report.out_time.toString())
    sheet.getRow(5).getCell(0).setCellValue(report.routine_desc)
    sheet.getRow(11).getCell(0).setCellValue(report.abnormal_desc)
    sheet.getRow(17).getCell(0).setCellValue(report.event_desc)

    val style = wb.createCellStyle();
    val font = wb.createFont();
    font.setFontHeightInPoints(12);
    font.setFontName("標楷體");

    style.setFont(font)

    var startRow = 24
    for {
      mt <- invalidHourList
      ih <- mt._2
    } {
      val row = sheet.createRow(startRow)
      def fillCell(i: Int, v: String) = {
        row.createCell(i).setCellStyle(style)
        row.getCell(i).setCellValue(v)
      }
      fillCell(0, MonitorType.map(mt._1).desp)
      fillCell(1, report.date.toString("MM/dd"))
      fillCell(2, ih.invalidHour)
      fillCell(3, ih.status)

      startRow += 1
    }
    finishExcel(reportFilePath, pkg, wb)
  }

  def minMonthlyReport(monitors: List[Monitor.Value], start: DateTime, callback: (Int) => Unit) = {
    val (reportFilePath, pkg, wb) = prepareTemplate("minMonthlyReport.xlsx")
    val evaluator = wb.getCreationHelper().createFormulaEvaluator()

    val fgColors =
      {
        val sheet0 = wb.getSheetAt(0)
        val seqColors =
          for (col <- 1 to 5)
            yield sheet0.getRow(1).getCell(col).getCellStyle.getFillForegroundXSSFColor
        seqColors.toArray
      }

    callback(20)
    for ((m, idx) <- monitors.zipWithIndex) {
      val minRecords = Record.getMinRecords(m, start, start + 1.month)
      val sheet = wb.createSheet(Monitor.map(m).name)
      sheet.createRow(0).createCell(0).setCellValue("時間")
      val timeSeries = minRecords.map { Record.timeProjection }
      for { (time, time_idx) <- timeSeries.zipWithIndex } {
        val row = sheet.createRow(time_idx + 1)
        val time_cell = row.createCell(0)
        time_cell.setCellValue(time.toString("YYYY/MM/dd HH:mm"))
      }

      for {
        (mt, mt_idx) <- Monitor.map(m).monitorTypes.zipWithIndex
        unit1 = sheet.getRow(0).createCell(mt_idx * 2 + 1).setCellValue(MonitorType.map(mt).desp)
        unit2 = sheet.getRow(0).createCell(mt_idx * 2 + 2).setCellValue("狀態碼")
        mtRecords = minRecords.map { Record.monitorTypeProject2(mt) }
        normalStyle = createStyle(mt)(wb)
        abnormalStyles = createColorStyle(fgColors, mt)(wb)
      } {

        val progress = 20 + 80 * (mt_idx + 1) / Monitor.map(m).monitorTypes.length
        callback(progress)

        for {
          (rec, rec_idx) <- mtRecords.zipWithIndex
        } {
          val row = sheet.getRow(rec_idx + 1)
          val vCell = row.createCell(mt_idx * 2 + 1)
          val sCell = row.createCell(mt_idx * 2 + 2)
          val valueOpt = rec._1
          val statusOpt = rec._2

          if (valueOpt.isEmpty || statusOpt.isEmpty) {
            vCell.setCellValue("-")
            sCell.setCellValue("-")
          } else {
            val value = valueOpt.get
            val status = statusOpt.get
            vCell.setCellValue(value)
            sCell.setCellValue(status)
            val cellStyle = getStyle(status, normalStyle, abnormalStyles)
            vCell.setCellStyle(cellStyle)
          }
        }
      }
    }
    wb.setActiveSheet(1)
    finishExcel(reportFilePath, pkg, wb)
  }

  def epbNotification(tickets: List[Ticket]) = {
    val (reportFilePath, pkg, wb) = prepareTemplate("epb.xlsx")
    val evaluator = wb.getCreationHelper().createFormulaEvaluator()

    var sheetN = 0
    for (t <- tickets) {
      if (sheetN != 0) {
        wb.cloneSheet(0)
      }
      val sheet = wb.getSheetAt(sheetN)
      //發生時間
      if (t.ticketType == TicketType.repair) {
        sheet.getRow(3).getCell(1).setCellValue(t.submit_date.toString("YYYY/MM/dd HH:mm"))
      } else {
        sheet.getRow(3).getCell(1).setCellValue(t.executeDate.toString("YYYY/MM/dd") + " 9:00:00 AM")
      }
      //發生地點
      sheet.getRow(4).getCell(1).setCellValue(s"台塑空氣品質監測站-${Monitor.map(t.monitor).name}")

      //事故說明 
      sheet.getRow(10).getCell(1).setCellValue(s"執行空氣品質監測站 - ${TicketType.map(t.ticketType)}")

      //結束時間
      if (t.ticketType == TicketType.repair) {
        sheet.getRow(12).getCell(1).setCellValue((t.submit_date + 8.hour).toString("YYYY/MM/dd HH:mm"))
      } else {
        sheet.getRow(12).getCell(1).setCellValue(t.executeDate.toString("YYYY/MM/dd") + " 5:00:00 PM")
      }
      sheetN += 1
    }
    wb.setActiveSheet(0)
    finishExcel(reportFilePath, pkg, wb)
  }

  def equipmentHistoryReport(tickets: List[Ticket], start: DateTime, end: DateTime) = {
    val (reportFilePath, pkg, wb) = prepareTemplate("equipHistory.xlsx")
    val evaluator = wb.getCreationHelper().createFormulaEvaluator()
    val sheet = wb.getSheetAt(0)

    sheet.getRow(1).getCell(6).setCellValue("起始日期:" + start.toString("YYYY/MM/dd"))
    sheet.getRow(2).getCell(6).setCellValue("結束日期:" + end.toString("YYYY/MM/dd"))

    for (tz <- tickets.zipWithIndex) {
      val t = tz._1
      val rowN = tz._2 + 4
      val row = sheet.getRow(rowN)
      row.getCell(0).setCellValue(Monitor.map(t.monitor).name)
      row.getCell(1).setCellValue(MonitorType.map(t.monitorType.get).desp)
      row.getCell(2).setCellValue(t.getRepairForm.start)
      row.getCell(3).setCellValue(t.getRepairForm.end)
      row.getCell(4).setCellValue(t.getRepairForm.explain + t.getRepairForm.result)
      row.getCell(5).setCellValue(t.getRepairForm.equipmentId)
      if (t.getRepairForm.parts.length == 0) {
        row.getCell(6).setCellValue("無")
      } else {
        row.getCell(6).setCellValue(t.getRepairForm.parts.map(_.id).mkString(","))
      }

    }
    finishExcel(reportFilePath, pkg, wb)
  }

}