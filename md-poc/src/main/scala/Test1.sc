case class AttrBuilder (name:String) {
}

trait ObjType {
  case class Attr(name:String)

  def name: String
  def a1: Attr
  def a2: Attr
}

case class GenericObjType (name:String, a1Builder:AttrBuilder, a2Builder:AttrBuilder) extends ObjType {
  def a1:Attr = Attr(a1Builder.name)
  def a2:Attr = Attr(a2Builder.name)
}

case class MdTypeBuilder (name:String) {

  var a1Builder:AttrBuilder = null
  var a2Builder:AttrBuilder = null

  def setA1(v:AttrBuilder) = {a1Builder = v;this}
  def setA2(v:AttrBuilder) = {a2Builder = v;this}
  def build = GenericObjType(name, a1Builder, a2Builder)
}

case class DbObject[T<:ObjType](objectType:T) {
  def getValue(attr:objectType.Attr) = "Value"
}

val t1 = MdTypeBuilder("Type1").setA1(AttrBuilder("t1a1")).setA2(AttrBuilder("t1a2")).build
val t2 = MdTypeBuilder("Type2").setA1(AttrBuilder("t2a1")).setA2(AttrBuilder("t2a2")).build

val o1 = DbObject[t1.type](t1)
val o2 = DbObject[t2.type](t2)

o1.getValue(t1.a1)
o2.getValue(t2.a1)

o1.getValue(t2.a1)



