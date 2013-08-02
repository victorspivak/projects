package svl.scalaimp.defs

object _Defs_ {
    implicit def ObjectToOption[T](value: T): Option[T] = {
        if (value == null)
            None
        else
            Some(value)
    }

    def option2String: Option[Any] => Option[String] = {
        case Some(value) => Some(value.toString)
        case None => None
    }
}
