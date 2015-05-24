package controllers
import play.api._
import play.api.mvc.Security._
import play.api.mvc._
import scala.concurrent._

class AuthenticatedRequest[A](val userinfo:String, request: Request[A]) extends WrappedRequest[A](request)

object Security {    
  val credentialKey = "credential"
  
  def getUserinfo(request: RequestHeader) = {
    request.session.get(credentialKey)
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
  
  def Authenticated = new AuthenticatedBuilder(getUserinfo, onUnauthorized)
  
  def setUserinfo[A](request: Request[A], userinfo:String)={
    if(userinfo.length() != 0)
      request.session + (credentialKey -> userinfo)
    else
      request.session - credentialKey
  }
  
  def getUserInfo[A]()(implicit request:Request[A]){
    request.session.get(credentialKey)
  }
}