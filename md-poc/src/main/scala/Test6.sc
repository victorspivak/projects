import scala.reflect.ClassTag

case class AttrBuilder (name:String)

case class ObjType (name:String, a1Builder:AttrBuilder, a2Builder:AttrBuilder) {
  case class Attr(name:String)
  case class DbObject(objType:ObjType) {
    def getValue(attr:Attr) = "Value"
    def getValue(attr:objType.Attr)(implicit dataType:ClassTag[objType.Attr]) = "Value"
    def foo() = println("Foo")
  }

  def a1 = this.Attr(a1Builder.name)
  def a2 = this.Attr(a2Builder.name)

  def buildObject() = DbObject(this)
}
case class MdBuilder (name:String) {
  var a1Builder:AttrBuilder = null
  var a2Builder:AttrBuilder = null

  def setA1(v:AttrBuilder) = {a1Builder = v;this}
  def setA2(v:AttrBuilder) = {a2Builder = v;this}
  def buildType = ObjType(name, a1Builder, a2Builder)
}

def dump(obj:ObjType#DbObject): Unit = {
  println("dump: " + obj.getValue(obj.objType.a1))
}

def dump1(t:ObjType, obj:ObjType#DbObject): Unit = {
  println("dump1: " + obj.asInstanceOf[t.DbObject].getValue(t.a1))
}

//// List[(t#DbObject[t]) forSome {type t >: GenericObjType : GenericObjType}]
//
//def dump11[T <: ObjType#DbObject](t:ObjType, obj:T forSome {type T <: t.DbObject}): Unit = {
//  println("dump11: " + obj.asInstanceOf[t.DbObject].getValue(t.a1))
//}

val t1 = MdBuilder("Type1").setA1(AttrBuilder("t1a1")).setA2(AttrBuilder("t1a2")).buildType
val t2 = MdBuilder("Type2").setA1(AttrBuilder("t2a1")).setA2(AttrBuilder("t2a2")).buildType
val o1 = t1.buildObject()
val o2 = t2.buildObject()

def dump2(obj:t1.DbObject): Unit = {
  println("dump2: " + obj.getValue(t1.a1))
}

o1.getValue(t1.a1)
o2.getValue(t2.a1)

o1.getValue(o1.objType.a1)
val t1a1 = o1.objType.a1
o1.getValue(t1a1)
val t2a1 = o2.objType.a1
o2.getValue(t2a1)

dump(o1)
dump1(t1, o1)
dump2(o1)
val types:List[ObjType] = t1 :: t1 :: Nil
val objs1 = List(t1.buildObject(), t1.buildObject())
objs1.foreach{obj =>
  println(obj.getValue(t1.a1))
}
val objs2 = types.map{t => t.buildObject()}
objs2.foreach{obj =>
  obj.objType match {
    case `t1` => println("t1:" + obj.asInstanceOf[t1.DbObject].getValue(t1.a1))
    case `t2` => println("t2:" + obj.asInstanceOf[t2.DbObject].getValue(t2.a1))
  }
}

objs2.foreach{obj =>
  println(obj.getValue(obj.objType.a1))
}
