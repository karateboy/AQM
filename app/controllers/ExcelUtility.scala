package controllers
import play.api._
import play.api.Play.current
import org.apache.poi.openxml4j.opc._
import org.apache.poi.xssf.usermodel._
import models.DailyReport
import models.Monitor
import models.MonitorStatus
import com.github.nscala_time.time.Imports._

/**
 * @author user
 */
object ExcelUtility {
  val docRoot = "/report_template/"
  
  def createDailyReport(monitor: Monitor.Value, reportDate:DateTime, data: DailyReport) = {
    import java.io._
    import java.nio.file.Files
    import java.nio.file._

    val templatePath = Paths.get(current.path.getAbsolutePath + docRoot + "daily_report.xlsx")
    val tempFilePath = Files.createTempFile("daily", ".xlsx");
    
    //Open Excel
    val pkg = OPCPackage.open(new FileInputStream(templatePath.toAbsolutePath().toString()))
    val wb = new XSSFWorkbook(pkg);
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
    val dateCell = titleRow.getCell(16)
    dateCell.setCellValue("資料日期:"+reportDate.toString("YYYY/MM/dd"))
/*    
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
        cell.setCellValue("%.2f".format(value))
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
*/
    Logger.debug(tempFilePath.toAbsolutePath().toString())
    val out = new FileOutputStream(tempFilePath.toAbsolutePath().toString());
    wb.write(out);
    out.close();
    pkg.close();
    
    new File(tempFilePath.toAbsolutePath().toString())
  }
}