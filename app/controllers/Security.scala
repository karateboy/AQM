package controllers
import play.api._
import play.api.mvc.Security._
import play.api.mvc._
import scala.concurrent._

class AuthenticatedRequest[A](val userinfo:String, request: Request[A]) extends WrappedRequest[A](request)

object Security {
  val idKey = "ID"
  val nameKey = "Name"
  val adminKey = "Admin"
  val groupIdKey = "GroupID"
  case class UserInfo(id:Int, name:String, isAdmin:Boolean, groupID:Int)
  

  def getUserinfo(request: RequestHeader):Option[UserInfo] = {
    val optId = request.session.get(idKey)
    if(optId.isEmpty)
      return None
      
    val optAdmin = request.session.get(adminKey)
    if(optAdmin.isEmpty)
      return None
      
    val optName = request.session.get(nameKey)
    if(optName.isEmpty)
      return None
    
    val optGroupID = request.session.get(groupIdKey)
    if(optGroupID.isEmpty)
      return None
      
    Some(UserInfo(optId.get.toInt, optName.get, optAdmin.get.toBoolean, optGroupID.get.toInt))
  }
  
  def onUnauthorized(request: RequestHeader) = {
    Logger.info("onUnauthorized")
    Results.Redirect(routes.Login.prompt()) 
  }
  
  //def invokeBlock[A](request: Request[A], block: (AuthenticatedRequest[A]) => Future[Result]) = {
  //  AuthenticatedBuilder(getUserinfo _, onUnauthorized)
  //})
  
  //def isAuthenticated(f: => String => Request[AnyContent] => Result) = {
  //  Authenticated(getUserinfo, onUnauthorized) { user =>
  //    Action(request => f(user)(request))
  //  }
  // }
  
  def setUserinfo[A](request: Request[A], userInfo:UserInfo)={
    request.session + 
      (idKey->userInfo.id.toString()) + (adminKey->userInfo.isAdmin.toString()) + 
      (nameKey->userInfo.name) + (groupIdKey->userInfo.groupID.toString()) 
  }
  
  def getUserInfo[A]()(implicit request:Request[A]):Option[UserInfo]={
    getUserinfo(request)
  }
  
  def Authenticated = new AuthenticatedBuilder(getUserinfo, onUnauthorized)
}