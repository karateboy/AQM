package controllers
import play.api._
import play.api.Play.current
import org.apache.poi.openxml4j.opc._
import org.apache.poi.xssf.usermodel._

/**
 * @author user
 */
object ExcelUtility {
  val docRoot = "/report_template/"
  def createDailyReport()={
    import java.io._
    import java.nio.charset.Charset
    import java.nio.file.Files
    import java.nio.file._
    

    val path = current.path.getAbsolutePath + docRoot + "daily_report.xlsx"
    Logger.debug(path)
    //val file = new File(path);
    //val filePath = file.toPath();
    //val tempFilePath = Files.createTempFile("daily", ".xlsx");
    //Files.copy(filePath, tempFilePath, StandardCopyOption.REPLACE_EXISTING);
    //Logger.debug(tempFilePath.toAbsolutePath().toString())
    //Open Excel
    val pkg = OPCPackage.open(path);
    val wb = new XSSFWorkbook(pkg);
    val sheet = wb.getSheetAt(0)
    val titleRow = sheet.getRow(2)
    val titleCell = titleRow.getCell(0)
    
    val calCell = titleRow.getCell(8)
    val calStyle = calCell.getCellStyle
    calStyle.getFillBackgroundColor
    
    
    titleCell.setCellStyle(calStyle)
    titleCell.setCellValue("測試站")
    
    val tempFilePath = Files.createTempFile("daily", ".xlsx");
    Logger.debug("temp file=>"+tempFilePath.toAbsolutePath().toString())
    val out = new FileOutputStream(tempFilePath.toAbsolutePath().toString());
    wb.write(out);
    out.close();
    pkg.close();
    Logger.debug("open and close successfully")
  }
}