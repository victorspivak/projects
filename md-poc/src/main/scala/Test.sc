case class Attr[T<:ObjType](objectType:T, name:String)

case class AttrBuilder (name:String) {
  def build [T<:ObjType] (objType:T) = new Attr(objType, name)
}

trait ObjType {
  def name: String
  def a1: Attr[ObjType]
  def a2: Attr[ObjType]
}

//case class MdTypeBuilder (name:String) {
//  var a1Builder:AttrBuilder = null
//  var a2Builder:AttrBuilder = null
//
//  def setA1(v:AttrBuilder) = {a1Builder = v;this}
//  def setA2(v:AttrBuilder) = {a2Builder = v;this}
//  def build = {
//    object MyType extends ObjType{
//      def name = name
//      def a1:Attr[MyType.type] = a1Builder.build(MyType)
//      def a2:Attr[MyType.type] = a2Builder.build(MyType)
//    }
//    MyType
//  }
//}

case class MdTypeBuilder (name:String) {
  var a1Builder:AttrBuilder = null
  var a2Builder:AttrBuilder = null

  def setA1(v:AttrBuilder) = {a1Builder = v;this}
  def setA2(v:AttrBuilder) = {a2Builder = v;this}
  def build = {
    case class GenericObjType private[MdTypeBuilder] () extends ObjType {
      def name = name
      def a1:Attr[this.type] = {
        val a = a1Builder.build[this.type](this)
        a
      }
      def a2:Attr[this.type] = a2Builder.build[this.type](this)
    }
    GenericObjType()
  }
}

object T3 extends ObjType{
  def name = "Type3"
  def a1:Attr[this.type] = AttrBuilder("t3a1").build(this)
  def a2:Attr[this.type] = AttrBuilder("t3a2").build(this)
}

object T4 extends ObjType{
  def name = "Type4"
  def a1 = AttrBuilder("t4a1").build(this)
  def a2 = AttrBuilder("t4a2").build(this)
}

case class DbObject[T<:ObjType](objectType:T) {
  def getValue(attr:Attr[T]) = "Value"
}

val t1 = MdTypeBuilder("Type1").setA1(AttrBuilder("t1a1")).setA2(AttrBuilder("t1a2")).build

val t2 = MdTypeBuilder("Type2").setA1(AttrBuilder("t2a1")).setA2(AttrBuilder("t2a2")).build

val o1 = DbObject[t1.type](t1)
val o2 = DbObject(t2)

val t1a1 = t1.a1
o1.getValue(t1a1)

val t3 = T3
val t4 = T4
val o3 = DbObject(t3)
val o4 = DbObject(t4)

val t3a1 = T3.a1
o3.getValue(T3.a1)
o3.getValue(T3.a2)


