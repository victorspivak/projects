package lib

/*
 * User: victor    Date: 11/5/13   Time: 2:52 AM
 */
object StringUtils {
  def diff(l:Seq[String]) = {
    l.reduceLeft{(found,str) =>
      val candidate = findDiff(found, str, "")
      if (candidate.length < found.length)
        candidate
      else
        found
    }
  }

  def findDiff(s1:String, s2:String, found:String):String = {
    if (s1.length() == 0 || s2.length() == 0 || (s1.charAt(0) != s2.charAt(0)))
      found
    else
      findDiff(s1.substring(1), s2.substring(1), found + s1.charAt(0))
  }
}
