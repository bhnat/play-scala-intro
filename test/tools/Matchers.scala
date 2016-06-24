package tools

import org.specs2.matcher.Expectable
import scala.concurrent.Future
import org.specs2.matcher.{Matcher,NotMatcher}
import play.api.mvc.Result

object MyMatchers {
  def haveHttpStatus(expected:Int) = new StatusMatcher(expected)
}

class StatusMatcher(expected:Int) extends Matcher[Option[Future[Result]]] {

  def apply[R <: Option[Future[Result]]](r: Expectable[R]) = {
    val v = r.value
    v match {
      case None => failure(s"${r.description} was None", r)
      case Some(fr:Future[Result]) =>
        import play.api.test.Helpers._
        val actual:Int = status(fr)
        result(actual == expected,
            s"${r.description} has status $actual as expected",
            s"${r.description} expected status $expected but found $actual",
            r)
      case _ =>
        failure(s"${r.description} has unexpected type $v", r)
    }
  }
}

trait NotHttpMatcher {
  implicit class NotStatusMatcherMatcher(result: NotMatcher[Option[Future[Result]]]) {
    def haveHttpStatus(expected:Int) = 
      MyMatchers.haveHttpStatus(expected).not
  }
}