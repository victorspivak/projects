case class AttrBuilder (name:String)

case class GenericObjType (name:String, a1Builder:AttrBuilder, a2Builder:AttrBuilder) {
  case class Attr(name:String)
  case class DbObject[T](objType:T) {
    def getValue(attr:Attr) = "Value"
    def foo() = println("Foo")
  }

  def a1 = this.Attr(a1Builder.name)
  def a2 = this.Attr(a2Builder.name)

  def buildObject() = DbObject[this.type](this)
}

case class MdBuilder (name:String) {
  var a1Builder:AttrBuilder = null
  var a2Builder:AttrBuilder = null

  def setA1(v:AttrBuilder) = {a1Builder = v;this}
  def setA2(v:AttrBuilder) = {a2Builder = v;this}
  def buildType = GenericObjType(name, a1Builder, a2Builder)
}

val t1 = MdBuilder("Type1").setA1(AttrBuilder("t1a1")).setA2(AttrBuilder("t1a2")).buildType
val t2 = MdBuilder("Type2").setA1(AttrBuilder("t2a1")).setA2(AttrBuilder("t2a2")).buildType

val o1 = t1.buildObject()
val o2 = t2.buildObject()

val t1a1 = t1.a1
val t2a1 = t2.a1

o1.getValue(t1.a1)
o2.getValue(t2.a1)
o1.getValue(t1a1)

val types:List[GenericObjType] = t1 :: t1 :: Nil

val objs = types.map{t => t.buildObject()}
objs.foreach{obj =>
  println(obj)
  //println(obj.getValue(t1.a1))
}
//o1.getValue(t2.a1)
