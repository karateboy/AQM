package models
import scalikejdbc._
import play.api._
import com.github.nscala_time.time.Imports._
import models.ModelHelper._
import models._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.libs.json.Json

case class Part(id:String, name:String, brand:String, model:String, equipment:String)
/**
 * @author user
 */
object Part {
  implicit val partRead = Json.reads[Part]
  def getList() = {
    DB readOnly { implicit session =>
      sql"""
        Select * 
        From Part
        """.map { r =>
        Part(r.string(1), r.string(2), r.string(3), r.string(4), r.string(5))
      }.list.apply
    }
  }
  
  def getPart(id:String) = {
    DB readOnly { implicit session =>
      sql"""
        Select * 
        From Part
        Where id = ${id}
        """.map { r =>
        Part(r.string(1), r.string(2), r.string(3), r.string(4), r.string(5))
      }.single.apply
    }
  }
  
  def getEquipPartMap()={
    import scala.collection.mutable.Map
    val parts = getList
    val map = Map.empty[String, List[Part]]
    for(p <- parts){
      val list = map.getOrElse(p.equipment, List.empty[Part])
      
      map.put(p.equipment, p::list)
    }
    map
  }

  def getIdNameMap()={
    val parts = getList
    val pairs=
      for(p<-parts)
        yield
        (p.id -> p.name)
    
    Map(pairs :_*)
  }
  def create(newPart:Part)={
    DB localTx { implicit session =>
      sql"""
        Insert into Part(id, name, brand, model, equipment)
        Values(${newPart.id}, ${newPart.name}, ${newPart.brand}, ${newPart.model}, ${newPart.equipment})
        """.update.apply
    }
  }
  
  def update(id:String, colName:String, newValue:String) = {    
    DB localTx { implicit session =>
      val col = SQLSyntax.createUnsafely(s"${colName}")
      sql"""
        Update Part
        Set ${col}=${newValue}
        Where id=${id}  
        """.update.apply
    }
  }

  def delete(id:String)={
    DB localTx { implicit session =>
      sql"""
        Delete from Part
        Where id = ${id}
        """.update.apply
    }
  }
}