package lib

/*
 * User: victor    Date: 11/5/13   Time: 2:52 AM
 */
object StringUtils {
  def diff(l:Seq[String]) = {
    if (!l.isEmpty){
      val pos = findDiffPos(l, 0)
      l.head.substring(0, pos)
    }
    else
      ""
  }

  private def findDiffPos(l:Seq[String], pos:Int):Int = {
    val first = l.head
    if (first.length == pos)
      pos
    else {
      val ch = first.charAt(pos)
      if (l.forall{str=>str.length > pos && str.charAt(pos) == ch})
        findDiffPos(l, pos + 1)
      else
        pos
    }
  }
}
