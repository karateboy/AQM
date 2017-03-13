package models
import scalikejdbc._
import play.api._
import com.github.nscala_time.time.Imports._
import models.ModelHelper._
import models._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.libs.json.Json

case class Part2(id: String, name: String, chineseName: String, brand: String, models: String, equipment: String,
                 unit: String, quantity: Int)
/**
 * @author user
 */
object Part {
  implicit val partWrite = Json.writes[Part2]
  implicit val partRead = Json.reads[Part2]
  private def mapper = (r: WrappedResultSet) => Part2(r.string("id"), r.string("name"), r.string("chineseName"), r.string("brand"),
    r.string("models"), r.string("equipment"), r.string("unit"), r.int("quantity"))

  def getList() = {
    DB readOnly { implicit session =>
      sql"""
        Select * 
        From Part2
        """.map { mapper }.list.apply
    }
  }

  def getPart(id: String) = {
    DB readOnly { implicit session =>
      sql"""
        Select * 
        From Part2
        Where id = ${id}
        """.map { mapper }.single.apply
    }
  }

  def getEquipModelPartMap() = {
    import scala.collection.mutable.Map
    val parts = getList
    val map = Map.empty[String, List[Part2]]
    for (p <- parts) {
      val models = p.models.split(",")
      for (model <- models) {
        val list = map.getOrElse(model, List.empty[Part2])
        map.put(model, p :: list)
      }
    }
    map
  }

  def getIdNameMap() = {
    val parts = getList
    val pairs =
      for (p <- parts)
        yield (p.id -> p.name)

    Map(pairs: _*)
  }
  
  def getIdMap() = {
    val parts = getList
    val pairs =
      for (p <- parts)
        yield (p.id -> p)

    Map(pairs: _*)
  }
  
  def create(newPart: Part2) = {
    DB localTx { implicit session =>
      sql"""
        Insert into Part2(id, name, chineseName, brand, models, equipment, unit, quantity)
        Values(${newPart.id}, ${newPart.name}, ${newPart.chineseName}, ${newPart.brand}, ${newPart.models}, ${newPart.equipment}, ${newPart.unit}, ${newPart.quantity})
        """.update.apply
    }
  }

  def update(id: String, colName: String, newValue: String) = {
    DB localTx { implicit session =>
      val col = SQLSyntax.createUnsafely(s"${colName}")
      if (colName != "quantity")
        sql"""
        Update Part2
        Set ${col}=${newValue}
        Where id=${id}  
        """.update.apply
      else {
        val newQuantity = newValue.toInt
        sql"""
        Update Part2
        Set ${col}=${newQuantity}
        Where id=${id}  
        """.update.apply
      }
    }
  }

  def delete(id: String) = {
    DB localTx { implicit session =>
      sql"""
        Delete from Part2
        Where id = ${id}
        """.update.apply
    }
  }
}