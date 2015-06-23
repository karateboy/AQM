package models
import scalikejdbc._
import play.api._
import com.github.nscala_time.time.Imports._
import models.ModelHelper._
import models._    

case class User(id:Option[Int], email:String, password:String, name:String, phone:String, isAdmin:Boolean, groupID:Int)

object User {
  def newUser(user:User)(implicit session: DBSession = AutoSession)={
    DB localTx { implicit session =>
      sql"""
        Insert into Users(email, name, password, phone, isAdmin, groupID)
        Values(${user.email}, ${user.name}, ${user.password}, ${user.phone}, ${user.isAdmin}, ${user.groupID})
        """.update.apply
    }
  }
  
  def deleteUser(id:Integer)(implicit session: DBSession = AutoSession)={
    DB localTx { implicit session =>
      sql"""
        Delete from Users
        Where id = ${id}
        """.update.apply
    }
  }
  
  def updateUser(user:User)(implicit session: DBSession = AutoSession)={
    assert(user.id.isDefined)
      DB localTx { implicit session =>
      sql"""
        Update Users
        Set email=${user.email}, name=${user.name}, password=${user.password}, phone=${user.phone}, isAdmin=${user.isAdmin}, groupID=${user.groupID}
        Where id=${user.id.get}  
        """.update.apply
    }
  }
  
  def getUserById(id:Int)(implicit session: DBSession = AutoSession)={
    sql"""
      Select *
      From Users
      Where id=${id}
      """.map { r => User(Some(r.int("ID")), r.string("Email"), 
          r.string("Password"), r.string("Name"), r.string("Phone"), r.boolean("isAdmin"),  
          r.int("GroupID")) }.single.apply()
  }
  
  def getUserByEmail(email:String)(implicit session: DBSession = AutoSession)={
    sql"""
      Select *
      From Users
      Where email=${email}
      """.map { r => User(Some(r.int("ID")), r.string("Email"), 
          r.string("Password"), r.string("Name"), r.string("Phone"), r.boolean("isAdmin"),  
          r.int("GroupID")) }.single.apply()
  }
  
  def getAllUsers()(implicit session: DBSession = AutoSession)={
    sql"""
      Select *
      From Users
      """.map { r => User(Some(r.int("ID")), r.string("Email"), 
          r.string("Password"), r.string("Name"), r.string("Phone"), r.boolean("isAdmin"),  
          r.int("GroupID")) }.list.apply()
  }
}

case class Group(id:Option[Int], name:String, privilege:String)
object Group {
  def newGroup(group: Group)(implicit session: DBSession = AutoSession) = {
    DB localTx { implicit session =>
      sql"""
        Insert into Groups(name, privilege)
        Values(${group.name}, ${group.privilege})
        """.update.apply
    }
  }
  
  def deleteGroup(id:Integer)(implicit session: DBSession = AutoSession)={
    DB localTx { implicit session =>
      sql"""
        Delete from Users
        Where id = ${id}
        """.update.apply
    }
  }
  
  def updateGroup(group:Group)(implicit session: DBSession = AutoSession)={
    assert(group.id.isDefined)
      DB localTx { implicit session =>
      sql"""
        Update Groups
        Set name=${group.name},privilege=${group.privilege}
        Where id=${group.id.get}  
        """.update.apply
    }
  }
  
  def getAllGroups()(implicit session: DBSession = AutoSession)={
    sql"""
      Select *
      From Groups
      """.map { r => Group(Some(r.int(1)), r.string(2), r.string(3))}.list.apply()
  }
}