package svl.test

import org.scalacheck._
import Arbitrary._
import Prop._
import lib.StringUtils

class QuickCheckStringUtils extends Properties("StringUtils") {
  property("diff1") = forAll { (str:String) =>
    val common = StringUtils.diff(List(str))

    common == str
  }

  property("diff2") = forAll { (strings:List[String], prefix:String) =>
    val stringsWithPrefix = strings.map(prefix + _)
    val common = StringUtils.diff(stringsWithPrefix)

    if (!strings.isEmpty)
      common.startsWith(prefix)
    else
      true
  }

  property("diff3") = forAll { (strings:List[String], prefix:String) =>
    val stringsWithPrefix = strings.map(prefix + _)
    val common = StringUtils.diff(stringsWithPrefix)

    if (!strings.isEmpty)
      stringsWithPrefix.forall(_.startsWith(common))
    else
      true
  }
}


