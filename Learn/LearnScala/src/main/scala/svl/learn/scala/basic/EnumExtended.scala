package svl.learn.scala.basic

import java.util.Date

object EnumExtended extends App{
    {//option 1
        object Suit extends Enumeration {
            val Clubs, Diamonds, Hearts, Spades = Value

            class SuitValue(suit: Value) {
                def isRed = !isBlack
                def isBlack = suit match {
                    case Clubs | Spades => true
                    case _              => false
                }
            }

            implicit def value2SuitValue(suit: Value) = new SuitValue(suit)
        }
        import Suit._
        println(Clubs.isRed)
    }

    {//option 2
        object Planet extends Enumeration {
            protected case class Val(name: String, mass: Double, radius: Double) extends super.Val(nextId, name) {
                def surfaceGravity: Double = Planet.G * mass / (radius * radius)
                def surfaceWeight(otherMass: Double): Double = otherMass * surfaceGravity
            }
            implicit def valueToPlanetVal(x: Value) = x.asInstanceOf[Val]

            val G: Double = 6.67300E-11
            val Mercury = Val("Mercury", 3.303e+23, 2.4397e6)
            val Venus   = Val("Venus", 4.869e+24, 6.0518e6)
            val Earth   = Val("Earth", 5.976e+24, 6.37814e6)
            val Mars    = Val("Mars", 6.421e+23, 3.3972e6)
            val Jupiter = Val("Jupiter", 1.9e+27, 7.1492e7)
            val Saturn  = Val("Saturn", 5.688e+26, 6.0268e7)
            val Uranus  = Val("Uranus", 8.686e+25, 2.5559e7)
            val Neptune = Val("Neptune", 1.024e+26, 2.4746e7)
        }

        import Planet._
        println(Earth.mass)
    }

    {
        abstract class MyEnumeration {
            // "Value" must be the name of the class defining your values type Value
            type Value

            // Contains your values in definition order
            private val vals = collection.mutable.LinkedHashMap[String, Value]()

            // A mixin for your values class to automatically collect the values
            protected trait ValuesCollector { self: Value =>
                private val ordinal = vals.size

                vals += (fieldNames(ordinal) -> self)

                def getName = fieldNames(ordinal)
                override def toString = getName
            }

            def apply(ordinal: Int) = vals(fieldNames(ordinal))
            def apply(fldName: String) = vals(fldName)

            def values = vals.values
            def namedValues: collection.Map[String, Value] = vals

            // Getting the field names through reflection.
            // Copied from scala.Enumeration
            private val fieldNames = getClass.getMethods filter (m =>
                m.getParameterTypes.isEmpty &&
                    classOf[ValuesCollector].isAssignableFrom(m.getReturnType) &&
                    m.getDeclaringClass != classOf[MyEnumeration]) map (_.getName)

        }

        object Planet extends MyEnumeration {

            case class Value(mass: Double, radius: Double) extends ValuesCollector {
                // universal gravitational constant  (m3 kg-1 s-2)
                private val G = 6.67300E-11;

                def surfaceGravity = G * mass / (radius * radius)
                def surfaceWeight(otherMass: Double) = otherMass * surfaceGravity

            }

            val MERCURY = Value(3.303e+23, 2.4397e6)
            val VENUS = Value(4.869e+24, 6.0518e6)
            val EARTH = Value(5.976e+24, 6.37814e6)
            val MARS = Value(6.421e+23, 3.3972e6)
            val JUPITER = Value(1.9e+27, 7.1492e7)
            val SATURN = Value(5.688e+26, 6.0268e7)
            val URANUS = Value(8.686e+25, 2.5559e7)
            val NEPTUNE = Value(1.024e+26, 2.4746e7)
            val PLUTO = Value(1.27e+22, 1.137e6)
        }

        import Planet._

        println(EARTH.mass)
    }

    {
//        object MdAttrDataType extends Enumeration {
//            case class Val[T](name: String) extends super.Val(nextId, name) {}
//            implicit def valueToMdAttrDataTypeVal(x: Value) = x.asInstanceOf[Val]
//
//            val StringType  = Val[String]("StringType")
//            val IntegerType = Val[Int]("IntegerType")
//            val DoubleType  = Val[Double]("DoubleType")
//            val DateType    = Val[Date]("DateType")
//
//            type MdAttrDataType = Val
//        }
//
//        import MdAttrDataType._
//
//        def foo[T](t:MdAttrDataType.Val[T], value:Any) = {
//            t match {
//                case StringType => value.asInstanceOf[String]
//                case IntegerType => value.asInstanceOf[Int]
//            }
//        }
//
//        val s = foo(StringType, "Hello")
//        val i = foo(IntegerType, 5)
//
//        println(s)
//        println(i)
//
    }

    {
        object MdAttrDataTypes{
            case class MdAttrDataType[T] (){}
            val StringType = MdAttrDataType[String]()
            val IntegerType = MdAttrDataType[Int]()
        }

        import MdAttrDataTypes._

        def foo[T](t:MdAttrDataType[T], value:Any):T = {
            t match {
                case StringType => value.asInstanceOf[T]
                case IntegerType => value.asInstanceOf[T]
            }
        }

        val s:String = foo(StringType, "Hello")
        val i = foo(IntegerType, 5)

        println(s)
        println(i)
    }
}
