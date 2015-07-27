package controllers
import play.api._
import play.api.Play.current
import org.apache.poi.openxml4j.opc._
import org.apache.poi.xssf.usermodel._
import models.DailyReport
import models.Monitor

/**
 * @author user
 */
object ExcelUtility {
  val docRoot = "/report_template/"
  def createDailyReport(monitor:Monitor.Value, data: DailyReport)={
    import java.io._
    import java.nio.charset.Charset
    import java.nio.file.Files
    import java.nio.file._
    

    val path = current.path.getAbsolutePath + docRoot + "daily_report.xlsx"
    Logger.debug(path)
    //Open Excel
    val pkg = OPCPackage.open(path);
    val wb = new XSSFWorkbook(pkg);
    val sheet = wb.getSheetAt(0)
    val titleRow = sheet.getRow(2)
    val titleCell = titleRow.getCell(0)
    
    val calColor = titleRow.getCell(8).getCellStyle.getFillBackgroundColor
    val repairColor = titleRow.getCell(9).getCellStyle.getFillBackgroundColor
    val maintanceColor = titleRow.getCell(10).getCellStyle.getFillBackgroundColor
    val invalidColor = titleRow.getCell(11).getCellStyle.getFillBackgroundColor
    val missingColor = titleRow.getCell(13).getCellStyle.getFillBackgroundColor
    val defaultStyle = sheet.getRow(6).getCell(1).getCellStyle
    val invalidStyle = sheet.getRow(6).getCell(1).getCellStyle
    
    titleCell.setCellValue("監測站:" + Monitor.map(monitor).name)
    
    for{col <- 1 to data.typeList.length
        row <- 4 to 28
        mtRecord = data.typeList(col - 1)
        cell = sheet.getRow(row).getCell(col)
        cellData = mtRecord.dataList(row - 4)}
    {
        if(cellData._3.isEmpty || cellData._3.isEmpty){
          cell.setCellValue("-")
          invalidStyle.setFillBackgroundColor(missingColor)
          cell.setCellStyle(invalidStyle)
        }
    }
    
    
    val tempFilePath = Files.createTempFile("daily", ".xlsx");
    Logger.debug("temp file=>"+tempFilePath.toAbsolutePath().toString())
    val out = new FileOutputStream(tempFilePath.toAbsolutePath().toString());
    wb.write(out);
    out.close();
    pkg.close();
    Logger.debug("open and close successfully")
    
  }
}