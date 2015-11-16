package models
import play.api._
import play.api.mvc._
import models.ModelHelper._
import scala.collection.Map
import scalikejdbc._
import play.api._
import com.github.nscala_time.time.Imports._
import models.ModelHelper._
import models._

import EnumUtils._

object SystemConfig{
  val AutoAuditAsNormal = "AutoAuditAsNormal"
  val DownloadLink = "DownloadLink"
  
  private val MaxFileLink = 10
  def getFreeDownloadLink():Int = {
    for(i <- 0 to MaxFileLink){
      val link = SystemConfig.getConfig(DownloadLink + i, "")
      if(link.length() == 0)
        return i
    }
    
    return 0
  }
  
  import java.io.File
  def setDownloadLink(n:Int, file:File){
    assert(n <= MaxFileLink)
    SystemConfig.setConfig(DownloadLink+n, file.getAbsolutePath)
  }
  def cleanDownloadLink(n:Int){
    assert(n <= MaxFileLink)
    SystemConfig.setConfig(DownloadLink+n, "")
  }
  
  def getDownloadLink(n:Int) = {
    assert(n <= MaxFileLink)
    val path = SystemConfig.getConfig(DownloadLink + n, "")
    new File(path)
  }
    
  var map = {
    val configPair =
      DB readOnly {
        implicit session =>

          sql"""
        Select * 
        From SystemConfig
        """.map { r => (r.string(1) -> r.string(2)) }.list.apply
      }
    Map(configPair: _*)
  }

  def getConfig(key:String, defaultValue:String)={
    val opt = map.get(key)
    if(opt.isDefined)
      opt.get
    else{
      newConfig(key, defaultValue)
      defaultValue
    }      
  }
  
  
  def setConfig(key:String, value:String)={
    map = (map - key) + (key->value)
    DB localTx {
      implicit session =>
        sql"""
          UPDATE SystemConfig
          SET [value] = ${value}
          WHERE configKey = ${key.toString}
          """.update.apply
    }
  }
  
  def newConfig(key:String, value:String)={
    DB localTx{
      implicit session =>
        sql"""
          INSERT INTO SystemConfig([configKey],[value])
          VALUES (${key}, ${value})
          """.update.apply
    }
    map += (key->value)
  }
}