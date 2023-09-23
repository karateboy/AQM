package controllers

import com.github.nscala_time.time.Imports._
import controllers.Report._
import models.ModelHelper._
import models.Record._
import models._
import org.apache.poi.openxml4j.opc._
import org.apache.poi.ss.usermodel._
import org.apache.poi.xssf.usermodel._
import play.api.Play.current
import play.api._

import java.io._
import java.nio.file._
import scala.math.BigDecimal.RoundingMode

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
    val format_str = if (prec != 0)
      "0." + "0" * prec
    else
      "0"

    val style = wb.createCellStyle();
    val format = wb.createDataFormat();
    // Create a new font and alter it.
    val font = wb.createFont();
    font.setFontHeightInPoints(10);
    font.setFontName("標楷體");

    style.setFont(font)
    style.setDataFormat(format.getFormat(format_str))
    style.setBorderBottom(BorderStyle.THIN);
    style.setBottomBorderColor(IndexedColors.BLACK.getIndex());
    style.setBorderLeft(BorderStyle.THIN);
    style.setLeftBorderColor(IndexedColors.BLACK.getIndex());
    style.setBorderRight(BorderStyle.THIN);
    style.setRightBorderColor(IndexedColors.BLACK.getIndex());
    style.setBorderTop(BorderStyle.THIN);
    style.setTopBorderColor(IndexedColors.BLACK.getIndex());
    style
  }

  def createStyle(prec:Int)(implicit wb: XSSFWorkbook) = {
    val format_str = if (prec != 0)
      "0." + "0" * prec
    else
      "0"

    val style = wb.createCellStyle();
    val format = wb.createDataFormat();
    // Create a new font and alter it.
    val font = wb.createFont();
    font.setFontHeightInPoints(10);
    font.setFontName("標楷體");

    style.setFont(font)
    style.setDataFormat(format.getFormat(format_str))
    style.setBorderBottom(BorderStyle.THIN);
    style.setBottomBorderColor(IndexedColors.BLACK.getIndex());
    style.setBorderLeft(BorderStyle.THIN);
    style.setLeftBorderColor(IndexedColors.BLACK.getIndex());
    style.setBorderRight(BorderStyle.THIN);
    style.setRightBorderColor(IndexedColors.BLACK.getIndex());
    style.setBorderTop(BorderStyle.THIN);
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
      case StatusType.Internal => {
        if (isValid(tag))
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
        if(info.auditRule.contains('M'))
          abnormalStyles(4)
        else
          normalStyle
    }
  }

  def historyReport(monitors: Seq[Monitor.Value], epaMonitors: Seq[EpaMonitor.Value], monitorType: MonitorType.Value, start: DateTime, end: DateTime, timeSeq: Seq[DateTime], recordMap: Map[Monitor.Value, Map[DateTime, (Option[Float], Option[String])]], epaRecordMap: Map[EpaMonitor.Value, Map[DateTime, Float]], showSec: Boolean = false, recordType: String = "Hour") = {
    implicit val (reportFilePath, pkg, wb) = prepareTemplate("historyReport.xlsx")
    val format = wb.createDataFormat();
    val sheet = wb.getSheetAt(0)
    val titleRow = sheet.getRow(2)
    val titleCell = titleRow.getCell(0)

    val fgColors = {
      val seqColors =
        for (col <- 1 to 5)
          yield sheet.getRow(3).getCell(col).getCellStyle.getFillForegroundXSSFColor
      seqColors.toArray
    }

    sheet.getRow(1).createCell(1).setCellValue(s"${start.toString("YYYY-MM-dd HH:mm")}~${end.toString("YYYY-MM-dd HH:mm")}")
    sheet.getRow(2).createCell(1).setCellValue(s"${MonitorType.map(monitorType).desp} (${MonitorType.map(monitorType).unit})")

    var col = 1
    for {m <- monitors} {
      sheet.getRow(4).createCell(col).setCellValue(s"${Monitor.map(m).name}")
      col += 1
    }

    for (epa <- epaMonitors) {
      sheet.getRow(4).createCell(col).setCellValue(s"${EpaMonitor.map(epa).name}")
      col += 1
    }

    var row = 5
    for (t <- timeSeq) {

      val timeStr =
        if (!showSec) {
          t.toString("YYYY-MM-dd HH:mm")
        } else {
          t.toString("YYYY-MM-dd HH:mm:ss")
        }

      val thisRow = sheet.createRow(row)
      thisRow.createCell(0).setCellValue(timeStr)

      var col = 1
      val normalStyle = createStyle(monitorType)
      val abnormalStyles = createColorStyle(fgColors, monitorType)
      for {
        m <- monitors
      } {
        val (valueOpt, statusOpt) = recordMap(m).getOrElse(t, (None, None))
        val cell = thisRow.createCell(col)
        if (valueOpt.isEmpty || statusOpt.isEmpty) {
          cell.setCellValue("-")
        } else {
          for {value <- valueOpt
               status <- statusOpt
               }{
            val d = BigDecimal(value.toDouble).setScale(MonitorType.map(monitorType).prec, RoundingMode.HALF_EVEN)
            cell.setCellValue(d.toDouble)
            val cellStyle = getStyle(status, normalStyle, abnormalStyles)
            cell.setCellStyle(cellStyle)
          }
        }
        col += 1
      }

      for {
        epa <- epaMonitors
        valueOpt = epaRecordMap(epa).get(t)
        cell = thisRow.createCell(col)
      } {
        if (valueOpt.isEmpty) {
          cell.setCellValue("-")
        } else {
          val value = valueOpt.get
          cell.setCellValue(value)
          cell.setCellStyle(normalStyle)
        }
        col += 1
      }
      row += 1
    }

    finishExcel(reportFilePath, pkg, wb)
  }

  def createAllDailyReport(reportDate: DateTime) = {
    implicit val (reportFilePath, pkg, wb) = prepareTemplate("all_daily_report.xlsx")
    val format = wb.createDataFormat();
    val sheet = wb.getSheetAt(0)
    val titleRow = sheet.getRow(2)
    val titleCell = titleRow.getCell(0)

    val fgColors = {
      val seqColors =
        for (col <- 3 to 7)
          yield titleRow.getCell(col).getCellStyle.getFillForegroundXSSFColor
      seqColors.toArray
    }

    def fillMonitorDailyReport(monitor: Monitor.Value, data: DailyReport, sheetIdx: Int) = {
      val sheet = wb.getSheetAt(sheetIdx)

      sheet.getRow(1).getCell(0).setCellValue("監測站:" + Monitor.map(monitor).name)
      sheet.getRow(1).getCell(30).setCellValue("查詢日期:" + DateTime.now.toString("YYYY/MM/dd"))
      sheet.getRow(2).getCell(30).setCellValue("資料日期:" + reportDate.toString("YYYY/MM/dd"))

      for {
        col <- 1 to data.typeList.length
        mtRecord = data.typeList(col - 1)
        normalStyle = createStyle(mtRecord.monitorType)
        abnormalStyles = createColorStyle(fgColors, mtRecord.monitorType)
        row <- 4 to 27
        cell = sheet.getRow(row).getCell(col)
        cellData = mtRecord.dataList(row - 4)
        prec = MonitorType.map(mtRecord.monitorType).prec
      } {
        val (_, valueOpt, statusOpt) = cellData
        if (valueOpt.isEmpty || statusOpt.isEmpty) {
          cell.setCellValue("-")
        } else {
          for {value <- valueOpt
               status <- statusOpt
               }{
            val d = BigDecimal(value.toDouble).setScale(prec, RoundingMode.HALF_EVEN)
            cell.setCellValue(d.toDouble)
            val cellStyle = getStyle(status, normalStyle, abnormalStyles)
            cell.setCellStyle(cellStyle)
          }
        }
      }

      for {
        col <- 1 to data.typeList.length
        mtRecord = data.typeList(col - 1)
        prec = MonitorType.map(mtRecord.monitorType).prec
      } {
        val stat = data.typeList(col - 1).stat

        def setter(v:Option[Float], rownum:Int, prec:Int): Unit ={
          v.fold(sheet.getRow(rownum).getCell(col).setCellValue("-"))(v =>
            {
              val cell = sheet.getRow(rownum)
                .getCell(col)
              cell.setCellValue(BigDecimal(v.toDouble).setScale(prec, RoundingMode.HALF_EVEN).toDouble)
              val normalStyle = createStyle(prec)
              cell.setCellStyle(normalStyle)
            })
        }
        setter(stat.avg, 28, 2)
        setter(stat.max, 29, prec)
        setter(stat.min, 30, prec)
        setter(stat.effectPercent, 31, 2)
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
      //wb.setSheetName(sheetIdx, Monitor.map(monitor).name)
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

    val fgColors = {
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
      prec = MonitorType.map(mtRecord.monitorType).prec
    } {
      val (date, valueOpt, statusOpt) = cellData
      if (valueOpt.isEmpty || statusOpt.isEmpty) {
        cell.setCellValue("-")
      } else {
        for {value <- valueOpt
             status <- statusOpt
             }{
          val d = BigDecimal(value.toDouble).setScale(prec, RoundingMode.HALF_EVEN)
          cell.setCellValue(d.toDouble)
          val cellStyle = getStyle(status, normalStyle, abnormalStyles)
          cell.setCellStyle(cellStyle)
        }
      }
    }

    for {
      col <- 1 to data.typeList.length
    } {
      val stat = data.typeList(col - 1).stat

      def fillOpt(vOpt: Option[Float], row: Int) {
        vOpt.fold(sheet.getRow(row).getCell(col).setCellValue("-"))(v => sheet.getRow(row).getCell(col).setCellValue(v))
      }

      fillOpt(stat.avg, 28)
      fillOpt(stat.max, 29)
      fillOpt(stat.min, 30)
      fillOpt(stat.effectPercent.map {
        _ * 100
      }, 31)
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

    val fgColors = {
      val seqColors =
        for (col <- 17 to 21)
          yield wb.getSheetAt(2).getRow(2).getCell(col).getCellStyle.getFillForegroundXSSFColor
      seqColors.toArray
    }

    def fillEffectSheet(sheet: XSSFSheet, monthHourReport: MonthHourReport) = {
      val titleRow = sheet.getRow(2)
      val titleCell = titleRow.getCell(0)
      titleCell.setCellValue("監測站:" + Monitor.map(monitor).name)
      sheet.getRow(1).getCell(30).setCellValue(reportDate.getYear)
      sheet.getRow(2).getCell(30).setCellValue(reportDate.getMonthOfYear)

      import org.apache.poi.hssf.util.HSSFColor
      def createInvalidStyle(mt: MonitorType.Value) = {
        val style = createStyle(mt)
        style.setFillForegroundColor(HSSFColor.HSSFColorPredefined.RED.getIndex)
        style.setFillPattern(FillPatternType.SOLID_FOREGROUND)
        style
      }

      for {
        col <- 1 to data.typeArray.length
        row <- 5 to (5 + nDay - 1)
        mtRecord = data.typeArray(col - 1)
        cell = sheet.getRow(row).getCell(col)
        cellData = mtRecord.dataList(row - 5)
        invalidStyle = createInvalidStyle(mtRecord.monitorType)
      } {
        cell.setCellValue(cellData.count)
        if (cellData.count < 16)
          cell.setCellStyle(invalidStyle)
      }
      //Hide unused Monitor Type
      for {
        col <- 1 to data.typeArray.length
        mtRecord = data.typeArray(col - 1)
      } {
        if (!Monitor.map(monitor).monitorTypes.contains(mtRecord.monitorType))
          sheet.setColumnHidden(col, true)
      }

      for {
        col <- 1 to data.typeArray.length
        mtRecord = data.typeArray(col - 1)
      } {
        val sum = mtRecord.dataList.map(_.count).sum
        sheet.getRow(36).getCell(col).setCellValue(sum)
        sheet.getRow(37).getCell(col).setCellValue(nDay * 24)
        val data = monthHourReport.dailyReports flatMap { dr =>
          dr.typeList(col - 1).dataList
        } map {
          _._3
        }

        import MonitorStatus._
        val actOfGodList = data.filter { p =>
          p.fold(false)(a => a.startsWith("m"))
        }

        val invalidList = data.filter { p =>
          p.isDefined &&
            !isValid(p.get)
        }
        val repairList = data.filter { p =>
          p.isDefined &&
            isRepairing(p.get)
        }
        sheet.getRow(39).getCell(col).setCellValue(actOfGodList.length)
        sheet.getRow(40).getCell(col).setCellValue(invalidList.length)
        sheet.getRow(42).getCell(col).setCellValue(repairList.length)
        sheet.getRow(44).getCell(col).setCellValue(nDay * 24)

        evaluator.evaluateFormulaCell(sheet.getRow(36).getCell(col))
        evaluator.evaluateFormulaCell(sheet.getRow(38).getCell(col))
        evaluator.evaluateFormulaCell(sheet.getRow(41).getCell(col))
        evaluator.evaluateFormulaCell(sheet.getRow(43).getCell(col))
        evaluator.evaluateFormulaCell(sheet.getRow(45).getCell(col))

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

      val abnormalColor = {
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
        if (cellData.avg.isDefined)
          cell.setCellValue(cellData.avg.get)
        else
          cell.setCellValue("-")

        if (cellData.count >= 16)
          cell.setCellStyle(normalStyle)
        else
          cell.setCellStyle(abnormalStyles(0))
      }

      for {
        col <- 1 to data.typeArray.length
        mtRecord = data.typeArray(col - 1)
      } {
        val stat = mtRecord.stat

        stat.avg.fold(sheet.getRow(36).getCell(col).setCellValue("-"))(avg => sheet.getRow(36).getCell(col).setCellValue(avg))
        stat.max.fold(sheet.getRow(37).getCell(col).setCellValue("-"))(max => sheet.getRow(37).getCell(col).setCellValue(max))
        stat.min.fold(sheet.getRow(38).getCell(col).setCellValue("-"))(min => sheet.getRow(38).getCell(col).setCellValue(min))
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
        (mt, idx) <- graph_list.zipWithIndex
        row = sheet.getRow(46)
      } {
        val mtCase = MonitorType.map(mt)
        val title =
          if (!Monitor.map(monitor).monitorTypes.contains(mt))
            s"${Monitor.map(monitor).name}無${mtCase.desp}測項"
          else if (MonitorTypeAlert.map(monitor)(mt).std_law.isDefined)
            s"${Monitor.map(monitor).name}${mtCase.desp}小時趨勢圖 (法規:${MonitorTypeAlert.map(monitor)(mt).std_law.get}${mtCase.unit})"
          else
            s"${Monitor.map(monitor).name}${mtCase.desp}小時趨勢圖 "

        row.getCell(idx).setCellValue(title)
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
          for {value <- valueOpt
               status <- statusOpt
               }{
            val d = BigDecimal(value.toDouble).setScale(MonitorType.map(mt).prec, RoundingMode.HALF_EVEN)
            cell.setCellValue(d.toDouble)
            val cellStyle = getStyle(status, normalStyle, abnormalStyles)
            cell.setCellStyle(cellStyle)
          }
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

        if (stat.avg.isDefined) {
          sheet.getRow(row).getCell(25).setCellValue(stat.avg.get)
          sheet.getRow(row).getCell(29).setCellValue(stat.avg.get * stat.count)
        } else {
          sheet.getRow(row).getCell(25).setCellValue("-")
          sheet.getRow(row).getCell(29).setCellValue("-")
        }
        sheet.getRow(row).getCell(26).setCellValue(stat.count)

        if (stat.max.isDefined)
          sheet.getRow(row).getCell(27).setCellValue(stat.max.get)
        else
          sheet.getRow(row).getCell(27).setCellValue("-")

        if (stat.min.isDefined)
          sheet.getRow(row).getCell(28).setCellValue(stat.min.get)
        else
          sheet.getRow(row).getCell(28).setCellValue("-")
      }

      //Day Stat
      for {
        sheetIndex <- 2 to reportTypeLen + 1
        sheet = wb.getSheetAt(sheetIndex)
        col <- 1 to 24
        row <- 4 to (4 + nDay - 1)
        stat = report.hourStatArray(col - 1)
      } {
        if (stat.count >= 20) {
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
          if (data._2.isDefined && data._3.isDefined && MonitorStatus.isValid(data._3.get)) {
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
          /*
          if (idxOpt.isDefined) {
            val graph_idx = idxOpt.get
            val std_internal = Monitor.map(monitor).getStdInternal(graph_idx._1)
            if (std_internal.isDefined) {
              sheet.getRow(row_start + idx).createCell(22 + graph_idx._2).setCellValue(std_internal.get)
            }
          }
          * /
          */
        }
        row_start += dayReport.typeList(0).dataList.length
      }
    }

    // 有效率月報
    fillMonthlySheet(wb.getSheetAt(1))
    val monthlyHourReport = monthlyHourReportHelper(monitor, reportDate)
    fillEffectSheet(wb.getSheetAt(0), monthlyHourReport)
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

    val abnormalColor = {
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
        data.avg.fold(cell.setCellValue("_"))(avg => cell.setCellValue(avg))
        if (data.count >= 20)
          cell.setCellStyle(normalStyle)
        else
          cell.setCellStyle(abnormalStyles(0))
      }
      val stat = report.typeArray(col - 1).stat

      def fillOpt(vOpt: Option[Float], idx: Int) {
        vOpt.fold({
          sheet.getRow(idx).getCell(col).setCellValue("-")
          sheet.getRow(idx).getCell(col).setCellStyle(abnormalStyles(0))
        })({ v =>
          sheet.getRow(idx).getCell(col).setCellStyle(normalStyle)
          sheet.getRow(idx).getCell(col).setCellValue(v)
        })
      }

      fillOpt(stat.avg, 16)
      fillOpt(stat.max, 17)
      fillOpt(stat.min, 18)
      fillOpt(stat.effectPercent, 19)
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
      def fillOpt(vOpt: Option[Float], idx: Int) {
        vOpt.fold(sheet.getRow(idx).getCell(m._2 + 1).setCellValue("-"))(v => sheet.getRow(idx).getCell(m._2 + 1).setCellValue(v * 100))
      }

      fillOpt(stat.min, 16)
      fillOpt(stat.max, 17)
      fillOpt(stat.avg, 18)
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

    val abnormalColor = {
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

      def fillOpt(r: Int, vOpt: Option[Float], idx: Int) {
        vOpt.fold({
          sheet.getRow(r).getCell(idx).setCellValue("-")
          sheet.getRow(r + 2).getCell(idx).setCellValue("-")
        })(v => {
          sheet.getRow(r).getCell(idx).setCellValue(v)
          sheet.getRow(r + 2).getCell(idx).setCellValue(covertDegToDir(v))
        })
      }
      {
        val stat = myMap(mt._1)._2
        fillOpt(row, stat.min, 26)
        fillOpt(row, stat.max, 27)
        fillOpt(row, stat.avg, 28)
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

      {
        val stat = epaMap(mt._1)._2
        fillOpt(row + 1, stat.min, 26)
        fillOpt(row + 1, stat.max, 27)
        fillOpt(row + 1, stat.avg, 28)
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
      } else if (Monitor.map(item.monitor).zdInternal(item.monitorType).isDefined && item.z_val > Monitor.map(item.monitor).zdInternal(item.monitorType).get) {
        fillCellF(newRow.createCell(3), item.z_val, 1)
      } else
        fillCellF(newRow.createCell(3), item.z_val, 0)

      if (Monitor.map(item.monitor).zdInternal(item.monitorType).isDefined)
        fillCellF(newRow.createCell(4), Monitor.map(item.monitor).zdInternal(item.monitorType).get, 0)

      if (MonitorType.map(item.monitorType).zd_law.isDefined)
        fillCellF(newRow.createCell(5), MonitorType.map(item.monitorType).zd_law.get, 0)

      fillCellF(newRow.createCell(6), item.s_std, 0)
      fillCellF(newRow.createCell(7), item.s_sval, 0)
      if (MonitorType.map(item.monitorType).sd_law.isDefined && item.sd_pnt > MonitorType.map(item.monitorType).sd_law.get) {
        fillCellF(newRow.createCell(8), item.sd_pnt, 2)
      } else if (Monitor.map(item.monitor).sdInternal(item.monitorType).isDefined && item.sd_pnt > Monitor.map(item.monitor).sdInternal(item.monitorType).get) {
        fillCellF(newRow.createCell(8), item.sd_pnt, 1)
      } else
        fillCellF(newRow.createCell(8), item.sd_pnt, 0)

      if (Monitor.map(item.monitor).sdInternal(item.monitorType).isDefined)
        fillCellF(newRow.createCell(9), Monitor.map(item.monitor).sdInternal(item.monitorType).get, 0)

      if (MonitorType.map(item.monitorType).sd_law.isDefined)
        fillCellF(newRow.createCell(10), MonitorType.map(item.monitorType).sd_law.get, 0)

      if (MonitorType.map(item.monitorType).zd_law.isDefined && MonitorType.map(item.monitorType).sd_law.isDefined) {
        if (item.z_val > MonitorType.map(item.monitorType).zd_law.get || item.sd_pnt > MonitorType.map(item.monitorType).sd_law.get) {
          fillCell(newRow.createCell(11), "失敗", 2)
        } else {
          if (item.z_val > Monitor.map(item.monitor).zdInternal(item.monitorType).get || item.sd_pnt > Monitor.map(item.monitor).sdInternal(item.monitorType).get) {
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
        itemOpt = map.get((row - 3).toString) if itemOpt.isDefined
        item = itemOpt.get
      } {
        sheet.getRow(row).getCell(1).setCellValue(item.startTime.toString("HH:mm"))
        sheet.getRow(row).getCell(2).setCellValue(MonitorType.map(item.monitorType).desp)
        for {
          zd_law <- MonitorType.map(item.monitorType).zd_law
          zd_internal <- Monitor.map(item.monitor).zdInternal(item.monitorType)
        } {
          if (item.z_val > zd_law)
            sheet.getRow(row).getCell(3).setCellStyle(lawStyle)
          else if (item.z_val > zd_internal)
            sheet.getRow(row).getCell(3).setCellStyle(internalStyle)

          sheet.getRow(row).getCell(4).setCellValue(zd_internal)
          sheet.getRow(row).getCell(5).setCellValue(zd_law)
        }

        sheet.getRow(row).getCell(3).setCellValue(item.z_val)
        sheet.getRow(row).getCell(6).setCellValue(item.s_std)
        sheet.getRow(row).getCell(7).setCellValue(item.s_sval)

        for {
          sd_law <- MonitorType.map(item.monitorType).sd_law
          sd_internal <- Monitor.map(item.monitor).sdInternal(item.monitorType)
        } {
          if (item.sd_pnt > sd_law)
            sheet.getRow(row).getCell(8).setCellStyle(lawStyle)
          else if (item.sd_pnt > sd_internal)
            sheet.getRow(row).getCell(8).setCellStyle(lawStyle)

          sheet.getRow(row).getCell(8).setCellValue(item.sd_pnt)
          sheet.getRow(row).getCell(9).setCellValue(sd_internal)
          sheet.getRow(row).getCell(10).setCellValue(sd_law)
        }

        for {
          zd_law <- MonitorType.map(item.monitorType).zd_law
          sd_law <- MonitorType.map(item.monitorType).sd_law
          zd_internal <- Monitor.map(item.monitor).zdInternal(item.monitorType)
          sd_internal <- Monitor.map(item.monitor).sdInternal(item.monitorType)
        } {
          if (item.z_val > zd_law || item.sd_pnt > sd_law) {
            sheet.getRow(row).getCell(11).setCellStyle(lawStyle)
            sheet.getRow(row).getCell(11).setCellValue("失敗")
          } else {
            if (item.z_val > zd_internal || item.sd_pnt > sd_internal) {
              sheet.getRow(row).getCell(11).setCellStyle(internalStyle)
            }

            sheet.getRow(row).getCell(11).setCellValue("成功")
          }
        }
      }
      sheet.getRow(36).getCell(0).setCellValue(s"${Monitor.map(monitor).name} (${MonitorType.map(monitorType).desp})零點校正趨勢圖")
      sheet.getRow(37).getCell(0).setCellValue(s"${Monitor.map(monitor).name} (${MonitorType.map(monitorType).desp})全幅校正趨勢圖")
      sheet.getRow(38).getCell(0).setCellValue(s"${Monitor.map(monitor).name} (${MonitorType.map(monitorType).desp})全幅讀值趨勢圖")
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
        } else if (item.z_val > Monitor.map(item.monitor).zdInternal(item.monitorType).get) {
          sheet.getRow(row).getCell(3).setCellStyle(internalStyle)
        }
        sheet.getRow(row).getCell(3).setCellValue(item.z_val)
        sheet.getRow(row).getCell(4).setCellValue(Monitor.map(item.monitor).zdInternal(item.monitorType).get)
        sheet.getRow(row).getCell(5).setCellValue(MonitorType.map(item.monitorType).zd_law.get)
        sheet.getRow(row).getCell(6).setCellValue(item.s_std)
        sheet.getRow(row).getCell(7).setCellValue(item.s_sval)
        if (item.sd_pnt > MonitorType.map(item.monitorType).sd_law.get) {
          sheet.getRow(row).getCell(8).setCellStyle(lawStyle)
        } else if (item.sd_pnt > Monitor.map(item.monitor).sdInternal(item.monitorType).get) {
          sheet.getRow(row).getCell(8).setCellStyle(lawStyle)
        }
        sheet.getRow(row).getCell(8).setCellValue(item.sd_pnt)
        sheet.getRow(row).getCell(9).setCellValue(Monitor.map(item.monitor).sdInternal(item.monitorType).get)
        sheet.getRow(row).getCell(10).setCellValue(MonitorType.map(item.monitorType).sd_law.get)
        if (item.z_val > MonitorType.map(item.monitorType).zd_law.get || item.sd_pnt > MonitorType.map(item.monitorType).sd_law.get) {
          sheet.getRow(row).getCell(11).setCellStyle(lawStyle)
          sheet.getRow(row).getCell(11).setCellValue("失敗")
        } else {
          if (item.z_val > Monitor.map(item.monitor).zdInternal(item.monitorType).get || item.sd_pnt > Monitor.map(item.monitor).sdInternal(item.monitorType).get) {
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
      val format_str = if (prec != 0)
        "0." + "0" * prec
      else
        "0"

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
          for (v <- pair(1)) {
            val d = BigDecimal(v).setScale(precArray(col - 1), RoundingMode.HALF_EVEN)
            cell.setCellValue(d.toFloat)
          }
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
          for (v <- pair(1)) {
            val d = BigDecimal(v).setScale(precArray(col - 1), RoundingMode.HALF_EVEN)
            cell.setCellValue(d.toDouble)
          }

          if (series.status.isDefined) {
            val statusCell = thisRow.createCell(pos + 1)
            pos += 1
            for (status <- series.status.get(row - 1))
              statusCell.setCellValue(status)
          }
        }
      }
    }

    finishExcel(reportFilePath, pkg, wb)
  }

  def exportWindRose(chart: HighchartData, monitorTypes: Array[MonitorType.Value]): File = {
    val precArray = monitorTypes.map { mt => MonitorType.map(mt).prec }

    val (reportFilePath, pkg, wb) = prepareTemplate("chart_export.xlsx")
    val evaluator = wb.getCreationHelper().createFormulaEvaluator()
    val format = wb.createDataFormat();

    val sheet = wb.getSheetAt(0)
    val headerRow = sheet.createRow(0)
    headerRow.createCell(0).setCellValue("風向")

    var pos = 0
    for {
      col <- 1 to chart.series.length
      series = chart.series(col - 1)
    } {
      headerRow.createCell(pos + 1).setCellValue(series.name)
      pos += 1
    }
    headerRow.createCell(pos + 1).setCellValue("總和")

    val style = {
      val format_str = "0.00" + "%"
      val style = wb.createCellStyle();
      style.setDataFormat(format.getFormat(format_str))
      style
    }

    // Categories data
    val windDirList = chart.xAxis.categories.get
    for (row <- windDirList.zipWithIndex) {
      val rowNo = row._2 + 1
      val thisRow = sheet.createRow(rowNo)
      thisRow.createCell(0).setCellValue(row._1)

      for {
        col <- 1 to chart.series.length
        series = chart.series(col - 1)
      } {
        val cell = thisRow.createCell(col)
        cell.setCellStyle(style)

        val pair = series.data(rowNo - 1)
        if (pair.length == 2 && pair(1).isDefined) {
          cell.setCellValue(pair(1).get / 100)
        }
      }
      val totalCell = thisRow.createCell(chart.series.length + 1)
      totalCell.setCellStyle(style)
      val total = chart.series.map {
        _.data(rowNo - 1)(1).get
      }.sum
      totalCell.setCellValue(total / 100)
    }
    val totalRow = sheet.createRow(windDirList.length + 1)
    totalRow.createCell(0).setCellValue("總和")
    for {
      col <- 1 to chart.series.length
      series = chart.series(col - 1)
    } {
      val cell = totalRow.createCell(col)
      cell.setCellStyle(style)

      val total = series.data.map {
        _ (1).get
      }.sum
      cell.setCellValue(total / 100)
    }
    val allTotalCell = totalRow.createCell(chart.series.length + 1)
    allTotalCell.setCellStyle(style)
    allTotalCell.setCellValue(1)

    finishExcel(reportFilePath, pkg, wb)
  }

  def exportWeekForm(ticket: Ticket, usrMap: Map[Int, User], oldTicketOpt: Option[Ticket] = None) = {
    val (reportFilePath, pkg, wb) = prepareTemplate("weekMaintance.xlsx")
    val evaluator = wb.getCreationHelper().createFormulaEvaluator()
    val format = wb.createDataFormat();

    val form = ticket.getForm
    val oldForm = oldTicketOpt.map { t => t.getForm }
    val sheet = wb.getSheetAt(0)
    val monitorName = Monitor.map(ticket.monitor).name
    sheet.getRow(1).getCell(1).setCellValue(monitorName)
    sheet.getRow(42).getCell(1).setCellValue(monitorName)
    sheet.getRow(75).getCell(1).setCellValue(monitorName)
    sheet.getRow(113).getCell(1).setCellValue(monitorName)

    val dateStr = ticket.executeDate.toString("YYYY/MM/d")
    sheet.getRow(1).getCell(5).setCellValue(dateStr)
    sheet.getRow(42).getCell(5).setCellValue(dateStr)
    sheet.getRow(75).getCell(5).setCellValue(dateStr)
    sheet.getRow(113).getCell(5).setCellValue(dateStr)
    oldTicketOpt.map {
      old =>
        val dateStr = old.executeDate.toString("YYYY/MM/d")
        sheet.getRow(1).getCell(3).setCellValue(dateStr)
    }

    val usrName = usrMap(ticket.owner_id).name
    sheet.getRow(2).getCell(5).setCellValue(usrName)
    sheet.getRow(43).getCell(5).setCellValue(usrName)
    sheet.getRow(76).getCell(5).setCellValue(usrName)
    sheet.getRow(114).getCell(5).setCellValue(usrName)

    sheet.getRow(3).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    sheet.getRow(4).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    sheet.getRow(6).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    sheet.getRow(7).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))

    oldForm.map { old => old.strIdx = form.strIdx }
    sheet.getRow(8).getCell(2).setCellValue(form.getStrSeq)
    oldForm.map { old => sheet.getRow(8).getCell(5).setCellValue(old.getStrSeq) }

    sheet.getRow(8).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    sheet.getRow(10).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))

    for (row <- 12 to 22) {
      sheet.getRow(row).getCell(2).setCellValue(form.getStrSeq)
      oldForm.map { old => sheet.getRow(row).getCell(5).setCellValue(old.getStrSeq) }
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }
    sheet.getRow(23).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    sheet.getRow(25).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    for (row <- 27 to 36) {
      sheet.getRow(row).getCell(2).setCellValue(form.getStrSeq)
      oldForm.map { old => sheet.getRow(row).getCell(5).setCellValue(old.getStrSeq) }
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }
    sheet.getRow(37).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))

    //Page 2
    sheet.getRow(44).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    for (row <- 46 to 51) {
      sheet.getRow(row).getCell(2).setCellValue(form.getStrSeq)
      oldForm.map { old => sheet.getRow(row).getCell(5).setCellValue(old.getStrSeq) }
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }
    sheet.getRow(52).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    sheet.getRow(54).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    for (row <- 56 to 59) {
      sheet.getRow(row).getCell(2).setCellValue(form.getStrSeq)
      oldForm.map { old => sheet.getRow(row).getCell(5).setCellValue(old.getStrSeq) }
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }
    sheet.getRow(60).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    sheet.getRow(62).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    for (row <- 64 to 67) {
      sheet.getRow(row).getCell(2).setCellValue(form.getStrSeq)
      oldForm.map { old => sheet.getRow(row).getCell(5).setCellValue(old.getStrSeq) }
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }
    sheet.getRow(68).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    sheet.getRow(69).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    sheet.getRow(70).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    //Page 3
    sheet.getRow(77).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    for (row <- 78 to 78) {
      sheet.getRow(row).getCell(2).setCellValue(form.getStrSeq)
      oldForm.map { old => sheet.getRow(row).getCell(5).setCellValue(old.getStrSeq) }
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }
    sheet.getRow(79).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    sheet.getRow(80).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    sheet.getRow(81).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    sheet.getRow(83).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    sheet.getRow(84).getCell(2).setCellValue("溫度：" + form.getStrSeq)
    sheet.getRow(84).getCell(3).setCellValue("濕度：" + form.getStrSeq)
    oldForm.map { old => sheet.getRow(84).getCell(5).setCellValue(old.getStrSeq + "/" + old.getStrSeq) }
    sheet.getRow(84).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    sheet.getRow(85).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    sheet.getRow(86).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    sheet.getRow(89).getCell(2).setCellValue(form.getStrSeq)
    oldForm.map { old => sheet.getRow(89).getCell(5).setCellValue(old.getStrSeq) }
    sheet.getRow(89).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    sheet.getRow(90).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    sheet.getRow(91).getCell(2).setCellValue("用電量：" + form.getStrSeq)
    oldForm.map { old => sheet.getRow(91).getCell(5).setCellValue(old.getStrSeq) }
    sheet.getRow(91).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    for (row <- 92 to 96) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }

    for (row <- 98 to 102)
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))

    for (row <- 104 to 105) {
      sheet.getRow(row).getCell(2).setCellValue(form.getStrSeq)
      oldForm.map { old => sheet.getRow(row).getCell(5).setCellValue(old.getStrSeq) }
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }
    for (row <- 106 to 108) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }

    //Page 4
    for (row <- 116 to 119) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }

    for (row <- 121 to 126) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }
    for (row <- 128 to 129) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
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

    val dateStr = ticket.executeDate.toString("YYYY/MM/d")
    sheet.getRow(1).getCell(11).setCellValue(dateStr)

    val usrName = usrMap(ticket.owner_id).name
    sheet.getRow(2).getCell(11).setCellValue(usrName)
    sheet.getRow(36).getCell(8).setCellValue(usrName)

    for (row <- 4 to 10) {
      sheet.getRow(row).getCell(1).setCellValue(form.getStrSeq)
      sheet.getRow(row).getCell(3).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }

    for (row <- 4 to 10) {
      sheet.getRow(row).getCell(4).setCellValue(form.getStrSeq)
      sheet.getRow(row).getCell(5).setCellValue(form.getStrSeq)
      sheet.getRow(row).getCell(6).setCellValue(form.getStrSeq)
      sheet.getRow(row).getCell(7).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
      sheet.getRow(row).getCell(8).setCellValue(form.getStrSeq)
      sheet.getRow(row).getCell(9).setCellValue(form.getStrSeq)
      sheet.getRow(row).getCell(10).setCellValue(form.getStrSeq)
      sheet.getRow(row).getCell(11).setCellValue(form.getStrSeq)
    }

    for (row <- 12 to 18) {
      sheet.getRow(row).getCell(7).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
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

    val dateStr = ticket.executeDate.toString("YYYY/MM/d")
    sheet.getRow(1).getCell(5).setCellValue(dateStr)
    sheet.getRow(35).getCell(5).setCellValue(dateStr)

    val usrName = usrMap(ticket.owner_id).name
    sheet.getRow(2).getCell(5).setCellValue(usrName)
    sheet.getRow(36).getCell(5).setCellValue(usrName)
    sheet.getRow(72).getCell(4).setCellValue(usrName)

    for (row <- 3 to 7) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }

    for (row <- 9 to 10) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }

    for (row <- 12 to 15) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }

    for (row <- 17 to 20) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }
    for (row <- 22 to 25) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }
    for (row <- 27 to 30) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }
    for (row <- 37 to 41) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }
    for (row <- 43 to 45) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }
    for (row <- 47 to 50) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }
    for (row <- 52 to 58) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }
    for (row <- 60 to 62) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
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

    val dateStr = ticket.executeDate.toString("YYYY/MM/d")
    sheet.getRow(1).getCell(5).setCellValue(dateStr)
    sheet.getRow(40).getCell(5).setCellValue(dateStr)

    val usrName = usrMap(ticket.owner_id).name
    sheet.getRow(2).getCell(5).setCellValue(usrName)
    sheet.getRow(41).getCell(5).setCellValue(usrName)
    sheet.getRow(64).getCell(4).setCellValue(usrName)

    sheet.getRow(3).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))

    for (row <- 5 to 7) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }

    for (row <- 9 to 14) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }

    for (row <- 16 to 19) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }

    for (row <- 21 to 24) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }
    for (row <- 26 to 30) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }
    for (row <- 32 to 35) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }
    for (row <- 42 to 46) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }
    for (row <- 48 to 50) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }
    for (row <- 52 to 54) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
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

    val dateStr = ticket.executeDate.toString("YYYY/MM/d")
    sheet.getRow(1).getCell(5).setCellValue(dateStr)
    sheet.getRow(27).getCell(5).setCellValue(dateStr)

    val usrName = usrMap(ticket.owner_id).name
    sheet.getRow(2).getCell(5).setCellValue(usrName)
    sheet.getRow(28).getCell(5).setCellValue(usrName)
    sheet.getRow(55).getCell(4).setCellValue(usrName)

    sheet.getRow(3).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))

    for (row <- 5 to 7) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }

    for (row <- 9 to 11) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }

    for (row <- 13 to 14) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }
    for (row <- 16 to 17) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }
    for (row <- 19 to 22) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }

    for (row <- 30 to 33) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }
    for (row <- 35 to 37) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }
    for (row <- 39 to 45) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
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

    val dateStr = ticket.executeDate.toString("YYYY/MM/d")
    sheet.getRow(1).getCell(5).setCellValue(dateStr)

    val usrName = usrMap(ticket.owner_id).name
    sheet.getRow(2).getCell(5).setCellValue(usrName)
    sheet.getRow(28).getCell(5).setCellValue(usrName)
    sheet.getRow(42).getCell(4).setCellValue(usrName)

    sheet.getRow(3).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    sheet.getRow(5).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))

    for (row <- 7 to 9) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }

    for (row <- 11 to 13) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }

    for (row <- 15 to 16) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }
    for (row <- 18 to 20) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }
    for (row <- 22 to 24) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }

    for (row <- 26 to 28) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }
    for (row <- 30 to 32) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
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

    //Attach photo
    val helper = wb.getCreationHelper();
    val anchor = helper.createClientAnchor();
    anchor.setCol1(1);
    anchor.setRow1(2);

    Ticket.getTicketPhoto(ticket.id).map { params =>
      for {
        photo_idx <- params.photos.zipWithIndex
        blobOpt = photo_idx._1 if blobOpt.isDefined
        idx = photo_idx._2
        blob = blobOpt.get
        photoSheet = wb.createSheet(s"照片$idx")
      } {
        import org.apache.commons.io._
        val is = blob.getBinaryStream
        val bytes = IOUtils.toByteArray(is)
        is.close
        val pictureIdx =
          if (bytes(0) == 0x89.toByte)
            wb.addPicture(bytes, Workbook.PICTURE_TYPE_PNG);
          else
            wb.addPicture(bytes, Workbook.PICTURE_TYPE_JPEG);

        val drawing = photoSheet.createDrawingPatriarch();
        val pict = drawing.createPicture(anchor, pictureIdx);
        pict.resize()
      }
    }

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

  def abnormalSummary(summaries: Seq[AbnormalSummary]) = {
    val (reportFilePath, pkg, wb) = prepareTemplate("abnormalSummary.xlsx")
    val evaluator = wb.getCreationHelper().createFormulaEvaluator()

    val sheet = wb.getSheetAt(0)
    for((summary, idx) <- summaries.zipWithIndex){
      val rowNum = idx + 2
      val row = sheet.createRow(rowNum)
      row.createCell(0).setCellValue(Monitor.map(summary.monitor).name)
      row.createCell(1).setCellValue(MonitorType.map(summary.monitorType).desp)
      row.createCell(2).setCellValue(summary.abnormalType)
      row.createCell(3).setCellValue(summary.duration)
      row.createCell(4).setCellValue(summary.count)
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

  def monitorJournalReport(report: MonitorJournal, entries: Seq[AbnormalEntry], userList: List[User]) = {
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
      entry <- entries
    } {
      val row = sheet.createRow(startRow)

      def fillCell(i: Int, v: String) = {
        row.createCell(i).setCellStyle(style)
        row.getCell(i).setCellValue(v)
      }

      fillCell(0, MonitorType.map(entry.monitorType).desp)
      fillCell(1, report.date.toString("MM/dd"))
      fillCell(2, entry.invalidHours)
      fillCell(3, entry.explain)

      startRow += 1
    }
    finishExcel(reportFilePath, pkg, wb)
  }

  def minMonthlyReport(monitors: List[Monitor.Value], start: DateTime, callback: (Int) => Unit) = {
    val (reportFilePath, pkg, wb) = prepareTemplate("minMonthlyReport.xlsx")
    val evaluator = wb.getCreationHelper().createFormulaEvaluator()

    val fgColors = {
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
      val timeSeries = minRecords.map {
        Record.timeProjection
      }
      for {(time, time_idx) <- timeSeries.zipWithIndex} {
        val row = sheet.createRow(time_idx + 1)
        val time_cell = row.createCell(0)
        time_cell.setCellValue(time.toString("YYYY/MM/dd HH:mm"))
      }

      for {
        (mt, mt_idx) <- Monitor.map(m).monitorTypes.zipWithIndex
        unit1 = sheet.getRow(0).createCell(mt_idx * 2 + 1).setCellValue(MonitorType.map(mt).desp)
        unit2 = sheet.getRow(0).createCell(mt_idx * 2 + 2).setCellValue("狀態碼")
        mtRecords = minRecords.map {
          Record.monitorTypeProject2(mt)
        }
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
            for {value <- valueOpt
                 status <- statusOpt
                 }{
              val d = BigDecimal(value.toDouble).setScale(MonitorType.map(mt).prec, RoundingMode.HALF_EVEN)
              vCell.setCellValue(d.toDouble)
              val cellStyle = getStyle(status, normalStyle, abnormalStyles)
              vCell.setCellStyle(cellStyle)
              sCell.setCellValue(status)
            }
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
      sheet.getRow(3).getCell(1).setCellValue(t.executeDate.toString("YYYY/MM/dd") + " 9:00:00 AM")

      //發生地點
      sheet.getRow(4).getCell(1).setCellValue(s"台塑空氣品質監測站-${Monitor.map(t.monitor).name}")

      //事故說明 
      sheet.getRow(10).getCell(1).setCellValue(s"執行空氣品質監測站 - ${TicketType.map(t.ticketType)}")

      //結束時間
      sheet.getRow(12).getCell(1).setCellValue(t.executeDate.toString("YYYY/MM/dd") + " 5:00:00 PM")

      sheetN += 1
    }
    wb.setActiveSheet(0)
    finishExcel(reportFilePath, pkg, wb)
  }

  def equipmentHistoryReport(equipment: Equipment, tickets: List[Ticket], start: DateTime, end: DateTime) = {
    val (reportFilePath, pkg, wb) = prepareTemplate("equipHistory.xlsx")
    val evaluator = wb.getCreationHelper().createFormulaEvaluator()
    val sheet = wb.getSheetAt(0)

    sheet.getRow(1).getCell(6).setCellValue("起始日期:" + start.toString("YYYY/MM/dd"))
    sheet.getRow(2).getCell(6).setCellValue("結束日期:" + end.toString("YYYY/MM/dd"))
    sheet.getRow(2).getCell(1).setCellValue(equipment.id)
    sheet.getRow(2).getCell(3).setCellValue(equipment.bought)
    sheet.getRow(2).getCell(4).setCellValue(s"品牌:${equipment.brand} 機型:${equipment.model} 序號:${equipment.serial}")
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

  def alarmList(arList: List[Alarm.Alarm], title: String) = {
    val (reportFilePath, pkg, wb) = prepareTemplate("alarm.xlsx")
    val evaluator = wb.getCreationHelper().createFormulaEvaluator()
    val sheet = wb.getSheetAt(0)

    sheet.getRow(0).getCell(0).setCellValue(title)
    for {
      ar_idx <- arList.zipWithIndex
      ar = ar_idx._1
      rowN = ar_idx._2 + 2
    } {
      val row = sheet.createRow(rowN)
      row.createCell(0).setCellValue(ar.time.toString("YYYY/MM/dd HH:mm"))
      row.createCell(1).setCellValue(Monitor.map(ar.monitor).name)
      row.createCell(2).setCellValue(Alarm.getItem(ar))
      row.createCell(3).setCellValue(Alarm.getReason(ar))
    }
    finishExcel(reportFilePath, pkg, wb)
  }

  def overStdAlarmList(arList: List[Alarm.Alarm], title: String) = {
    val (reportFilePath, pkg, wb) = prepareTemplate("overStdAlarm.xlsx")
    val evaluator = wb.getCreationHelper().createFormulaEvaluator()
    val sheet = wb.getSheetAt(0)

    sheet.getRow(0).getCell(0).setCellValue(title)
    for {
      ar_idx <- arList.zipWithIndex
      ar = ar_idx._1
      rowN = ar_idx._2 + 2
    } {
      val row = sheet.createRow(rowN)
      row.createCell(0).setCellValue(ar.time.toString("YYYY/MM/dd HH:mm"))
      row.createCell(1).setCellValue(Monitor.map(ar.monitor).name)
      row.createCell(2).setCellValue(Alarm.getItem(ar))
      row.createCell(3).setCellValue(Alarm.getMonitorTypeValue(ar))
      row.createCell(4).setCellValue(Alarm.getReason(ar))
    }
    finishExcel(reportFilePath, pkg, wb)
  }

  def aggregate2List(arList: List[AggregateReport2], title: String) = {
    val (reportFilePath, pkg, wb) = prepareTemplate("aggregate2.xlsx")
    val evaluator = wb.getCreationHelper().createFormulaEvaluator()
    val sheet = wb.getSheetAt(0)

    sheet.getRow(0).getCell(0).setCellValue(title)
    for {
      ar_idx <- arList.zipWithIndex
      ar = ar_idx._1
      rowN = ar_idx._2 + 2
    } {
      val row = sheet.createRow(rowN)
      row.createCell(0).setCellValue(ar.time.toString("YYYY/MM/dd HH:mm"))
      row.createCell(1).setCellValue(Monitor.map(ar.monitor).name)
      row.createCell(2).setCellValue(MonitorType.map(ar.monitorType).desp)
      row.createCell(3).setCellValue(MonitorType.format(ar.monitorType, Some(ar.value)))
      row.createCell(4).setCellValue(MonitorType.format(ar.monitorType, ar.stdLaw))
      row.createCell(5).setCellValue(MonitorType.format(MonitorType.C212, ar.windDir))
      row.createCell(6).setCellValue(ar.windAngle)
      row.createCell(7).setCellValue(MonitorType.format(MonitorType.C211, ar.windSpeed))
      row.createCell(8).setCellValue(ar.action)
      row.createCell(9).setCellValue(ar.state)
    }
    finishExcel(reportFilePath, pkg, wb)
  }

  def repairingTickets(tickets: List[(Ticket, Option[Alarm.Alarm])], title: String, userMap: Map[Int, User]) = {
    val (reportFilePath, pkg, wb) = prepareTemplate("repairingTicket.xlsx")
    val evaluator = wb.getCreationHelper().createFormulaEvaluator()
    val sheet = wb.getSheetAt(0)

    sheet.getRow(0).getCell(0).setCellValue(title)
    for {
      tx_idx <- tickets.zipWithIndex
      ticket_ar = tx_idx._1
      ticket = ticket_ar._1
      arOpt = ticket_ar._2
      rowN = tx_idx._2 + 2
    } {
      val row = sheet.createRow(rowN)
      row.createCell(0).setCellValue(ticket.id)
      if (arOpt.isDefined) {
        row.createCell(1).setCellValue(arOpt.get.time.toString("YYYY/MM/dd HH:mm"))
        row.createCell(2).setCellValue(MonitorStatus.map(arOpt.get.code).desp)
      }

      row.createCell(3).setCellValue(Monitor.map(ticket.monitor).name)
      if (ticket.monitorType.isDefined)
        row.createCell(4).setCellValue(MonitorType.map(ticket.monitorType.get).desp)

      row.createCell(5).setCellValue(ticket.submit_date.toString("MM-d HH:mm"))
      row.createCell(6).setCellValue(userMap(ticket.submiter_id).name)
      row.createCell(7).setCellValue(ticket.reason)
      row.createCell(8).setCellValue(userMap(ticket.owner_id).name)
      row.createCell(9).setCellValue(ticket.executeDate.toString("MM-d"))
      if (ticket.executeDate.isBefore(DateTime.yesterday)) {
        val state =
          if (ticket.extendDate.isEmpty)
            "已逾期"
          else {
            val extendDate = ticket.extendDate.get
            s"展延(${extendDate.toString("M/d")})${ticket.extendReason.getOrElse("")}"
          }
        row.createCell(10).setCellValue(state)
      } else
        row.createCell(10).setCellValue("否")
    }

    finishExcel(reportFilePath, pkg, wb)
  }

  def tickets(tickets: List[Ticket], title: String, userMap: Map[Int, User]) = {
    val (reportFilePath, pkg, wb) = prepareTemplate("ticket.xlsx")
    val evaluator = wb.getCreationHelper().createFormulaEvaluator()
    val sheet = wb.getSheetAt(0)

    sheet.getRow(0).getCell(0).setCellValue(title)
    for {
      ticket_idx <- tickets.zipWithIndex
      ticket = ticket_idx._1
      rowN = ticket_idx._2 + 2
    } {
      val row = sheet.createRow(rowN)
      row.createCell(0).setCellValue(ticket.id)

      row.createCell(1).setCellValue(Monitor.map(ticket.monitor).name)
      if (ticket.monitorType.isDefined)
        row.createCell(2).setCellValue(MonitorType.map(ticket.monitorType.get).desp)

      row.createCell(3).setCellValue(ticket.submit_date.toString("MM-d HH:mm"))
      row.createCell(4).setCellValue(userMap(ticket.submiter_id).name)
      row.createCell(5).setCellValue(TicketType.map(ticket.ticketType))
      row.createCell(6).setCellValue(ticket.reason)
      row.createCell(7).setCellValue(ModelHelper.formatOptStr(ticket.repairType))
      row.createCell(8).setCellValue(ModelHelper.formatOptStr(ticket.repairSubType))
      row.createCell(9).setCellValue(userMap(ticket.owner_id).name)
      row.createCell(10).setCellValue(ticket.executeDate.toString("MM-d"))
      val form = ticket.getRepairForm
      row.createCell(11).setCellValue(form.explain)
      row.createCell(12).setCellValue(form.result)
      val resultOptStr = List(
        if (form.getBool(3)) Some("儀器異常")
        else
          None,
        if (form.getBool(4)) Some("儀器無異常")
        else
          None,
        if (form.getBool(5)) Some("待料")
        else
          None)

      val result = resultOptStr.flatMap { x => x }.mkString("/")
      row.createCell(13).setCellValue(result)
      row.createCell(14).setCellValue(form.end)
    }

    finishExcel(reportFilePath, pkg, wb)
  }

  def closedRepairTickets(ticketWithRepairForm: List[(Ticket, RepairFormData)], title: String, userMap: Map[Int, User]) = {
    val (reportFilePath, pkg, wb) = prepareTemplate("closedRepairTicket.xlsx")
    val evaluator = wb.getCreationHelper().createFormulaEvaluator()
    val sheet = wb.getSheetAt(0)

    sheet.getRow(0).getCell(0).setCellValue(title)
    for {
      ticketR_idx <- ticketWithRepairForm.zipWithIndex
      (ticket, form) = ticketR_idx._1
      rowN = ticketR_idx._2 + 2
    } {
      val row = sheet.createRow(rowN)
      row.createCell(0).setCellValue(ticket.id)

      if (form.alarm.isDefined) {
        row.createCell(1).setCellValue(form.alarm.get.time.toString("YYYY/MM/dd HH:mm"))
        row.createCell(2).setCellValue(MonitorStatus.map(form.alarm.get.code).desp)
      } else {
        row.createCell(1).setCellValue("-")
        row.createCell(2).setCellValue("-")
      }
      row.createCell(3).setCellValue(Monitor.map(ticket.monitor).name)
      if (ticket.monitorType.isDefined)
        row.createCell(4).setCellValue(MonitorType.map(ticket.monitorType.get).desp)

      row.createCell(5).setCellValue(ticket.submit_date.toString("MM-d HH:mm"))
      row.createCell(6).setCellValue(userMap(ticket.submiter_id).name)
      row.createCell(7).setCellValue(ticket.reason)
      row.createCell(8).setCellValue(userMap(ticket.owner_id).name)
      row.createCell(9).setCellValue(form.explain)
      row.createCell(10).setCellValue(form.result)
      val resultOptStr = List(
        if (form.getBool(3)) Some("儀器異常")
        else
          None,
        if (form.getBool(4)) Some("儀器無異常")
        else
          None,
        if (form.getBool(5)) Some("待料")
        else
          None)

      val result = resultOptStr.flatMap { x => x }.mkString("/")
      row.createCell(11).setCellValue(result)
      row.createCell(12).setCellValue(form.end)
    }

    finishExcel(reportFilePath, pkg, wb)
  }

  def parts(partList: List[Part2], title: String) = {
    val (reportFilePath, pkg, wb) = prepareTemplate("parts.xlsx")
    val evaluator = wb.getCreationHelper().createFormulaEvaluator()
    val sheet = wb.getSheetAt(0)

    sheet.getRow(0).getCell(0).setCellValue(title)
    for {
      part_idx <- partList.zipWithIndex
      p = part_idx._1
      rowN = part_idx._2 + 2
    } {
      val row = sheet.createRow(rowN)
      row.createCell(0).setCellValue(p.id)
      row.createCell(1).setCellValue(p.name)
      row.createCell(2).setCellValue(p.chineseName)
      row.createCell(3).setCellValue(p.brand)
      row.createCell(4).setCellValue(p.models)
      row.createCell(5).setCellValue(p.equipment)
      row.createCell(6).setCellValue(p.unit)
      row.createCell(7).setCellValue(p.quantity)
    }
    finishExcel(reportFilePath, pkg, wb)
  }

  def partUsage(partUsageList: List[PartUsage], partMap: Map[String, Part2], title: String) = {
    val (reportFilePath, pkg, wb) = prepareTemplate("partUsage.xlsx")
    val evaluator = wb.getCreationHelper().createFormulaEvaluator()
    val sheet = wb.getSheetAt(0)

    sheet.getRow(0).getCell(0).setCellValue(title)
    for {
      part_idx <- partUsageList.zipWithIndex
      p = part_idx._1
      rowN = part_idx._2 + 2
    } {
      val row = sheet.createRow(rowN)
      row.createCell(0).setCellValue(p.id)
      row.createCell(1).setCellValue(partMap(p.getPartId).name)
      row.createCell(2).setCellValue(partMap(p.getPartId).chineseName)
      row.createCell(3).setCellValue(Monitor.map(Monitor.withName(p.monitor)).name)
      row.createCell(4).setCellValue(p.usage)
      row.createCell(5).setCellValue(p.getFreqDesc)
      row.createCell(6).setCellValue(p.startDate.toString("YYYY-MM-dd"))
      row.createCell(7).setCellValue(p.getNextReplaceDate.toString("YYYY-MM-dd"))
      row.createCell(8).setCellValue(ModelHelper.formatOptBool(Some(p.alarm)))
      row.createCell(9).setCellValue(ModelHelper.formatOptStr(p.nochange_reason))
      row.createCell(10).setCellValue(ModelHelper.formatOptStr(p.remark))
    }
    finishExcel(reportFilePath, pkg, wb)
  }

  import Maintance.PartInventory

  def partInventoryAlarm(partList: List[PartInventory], idMap: Map[String, Part2], title: String) = {
    val (reportFilePath, pkg, wb) = prepareTemplate("partInventoryAlarm.xlsx")
    val evaluator = wb.getCreationHelper().createFormulaEvaluator()
    val sheet = wb.getSheetAt(0)

    sheet.getRow(0).getCell(0).setCellValue(title)
    for {
      part_idx <- partList.zipWithIndex
      p = part_idx._1
      rowN = part_idx._2 + 2
    } {
      val row = sheet.createRow(rowN)
      row.createCell(0).setCellValue(p.id)
      row.createCell(1).setCellValue(idMap(p.id).name)
      row.createCell(2).setCellValue(idMap(p.id).chineseName)
      row.createCell(3).setCellValue(p.inventory)
      row.createCell(4).setCellValue(p.usage)
    }
    finishExcel(reportFilePath, pkg, wb)
  }
}