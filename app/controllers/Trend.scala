package controllers
import play.api._
import play.api.mvc._
import play.api.Logger
import models._
import com.github.nscala_time.time.Imports._

object Trend extends Controller {
  def trendReport = Security.Authenticated{
    Ok(views.html.trendReport())
  }
  
}