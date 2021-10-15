package models

import play.api.libs.json.Json
import scalikejdbc._

case class Part2(id: String, name: String, chineseName: String, brand: String, models: String, equipment: String,
                 unit: String, quantity: Int)

/**
 * @author user
 */
object Part {
  implicit val partWrite = Json.writes[Part2]
  implicit val partRead = Json.reads[Part2]

  def getPart(id: String)(implicit session: DBSession = AutoSession) = {

    sql"""
        Select *
        From Part2
        Where id = ${id}
        """.map {
      mapper
    }.single.apply

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

  def getList()(implicit session: DBSession = AutoSession): List[Part2] = {
    sql"""
        Select *
        From Part2
        """.map {
      mapper
    }.list.apply
  }

  private def mapper = (r: WrappedResultSet) => Part2(r.string("id"), r.string("name"), r.string("chineseName"), r.string("brand"),
    r.string("models"), r.string("equipment"), r.string("unit"), r.int("quantity"))

  def getIdMap() = {
    val parts = getList
    val pairs =
      for (p <- parts)
        yield (p.id -> p)

    Map(pairs: _*)
  }

  def create(newPart: Part2)(implicit session: DBSession = AutoSession) = {
    sql"""
        Insert into Part2(id, name, chineseName, brand, models, equipment, unit, quantity)
        Values(${newPart.id}, ${newPart.name}, ${newPart.chineseName}, ${newPart.brand}, ${newPart.models}, ${newPart.equipment}, ${newPart.unit}, ${newPart.quantity})
        """.update.apply
  }

  def update(id: String, colName: String, newValue: String)(implicit session: DBSession = AutoSession) = {

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

  def delete(id: String)(implicit session: DBSession = AutoSession) = {
    sql"""
        Delete from Part2
        Where id = ${id}
        """.update.apply
  }
}