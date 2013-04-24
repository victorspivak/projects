package svl.scala.lib.generators

import util.Random

trait AbstractAttributeGenerator {
  def next: String
}

class EntityTemplate(val name:String, val attrTemplates: List[(String, AbstractAttributeGenerator)]) {
    def generate (overwriteAttributes: Option[List[(String, String)]]) = {
        val overwritesIndex = overwriteAttributes.getOrElse(List()).toMap.map(e => (e._1.toLowerCase -> e._2))
        val attributes = attrTemplates.map{
            param => val (name_, gen) = param
            val name = name_.toLowerCase
            (name, if(overwritesIndex.contains(name)) overwritesIndex (name) else gen.next)
        }

        val attributesIndex = attributes.toMap.map(e => (e._1.toLowerCase -> e._2))
        attributes ++ overwriteAttributes.getOrElse(List()).filter(e => !attributesIndex.contains(e._1.toLowerCase))
    }

    def find(name:String, overwriteAttributes: Option[List[(String, String)]]) =
                  generate(overwriteAttributes).find(attr => attr._1.equalsIgnoreCase(name)).get._2
}

object EntityTemplate {
  def apply(name: String, attrs: (String, AbstractAttributeGenerator)*) = {
    new EntityTemplate(name, attrs.toList)
  }

  def toBase64 (str:String) = {
    new sun.misc.BASE64Encoder().encode(str.getBytes)
  }

  implicit def String2EntityTemplate(name:String) = EntityTemplate(name)

  class ConstString(val value:String) extends AbstractAttributeGenerator {
    def next:String = value
  }

  object ConstString {
    def apply(value:String) = new ConstString(value)
  }

  implicit def String2ConstString(value:String) = ConstString(value)

  class Based64String(val generator:AbstractAttributeGenerator) extends AbstractAttributeGenerator {
    def next:String = toBase64(generator.next)
  }

  object Based64String {
    def apply(generator:AbstractAttributeGenerator) = new Based64String(generator)
  }

  class AutoString(val template: String, val mask: String, var sid: Int = 0) extends AbstractAttributeGenerator {
    val maskLength = mask.length

    def next: String = {
      val str = mask + sid takeRight maskLength
      sid += 1
      template.format(str)
    }
  }

  object AutoString {
    def apply(template: String, mask: String, sid: Int = 0) = new AutoString(template, mask, sid)
  }

  class AutoStringTimerBased(val template: String, val mask: String) extends AbstractAttributeGenerator {
    val maskLength = mask.length

    def next: String = {
      template.format(mask + System.currentTimeMillis() takeRight maskLength)
    }
  }

  object AutoStringTimerBased {
    def apply(template: String, mask: String) = new AutoStringTimerBased(template, mask)
  }

  class RandomString(val template: String, val mask: String, val random: Random = RandomString.defaultRandom) extends AbstractAttributeGenerator {
    val maskLength = mask.length

    private def nextChar: Char = {
      val c = random.nextPrintableChar()
      if (!RandomString.isAllowed(c)) nextChar else c
    }

    def nextLength = random.nextInt(maskLength)

    def next: String = {
      val length = nextLength
      val str = mask + (1 to length).map(s => nextChar).mkString takeRight maskLength
      template.format(str)
    }
  }

  object RandomString {
    private def charFilter = Set() ++ ('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9') +('#', '@')

    val defaultRandom = new Random

    def apply(template: String, mask: String, random: Random = defaultRandom) = new RandomString(template, mask, random)

    def isAllowed(c: Char) = charFilter.contains(c)
  }

  class RandomFixedString(template: String, mask: String, random: Random = RandomString.defaultRandom) extends
  RandomString(template, mask, random) {

    override def nextLength = maskLength
  }

  object RandomFixedString {
    def apply(template: String, mask: String, random: Random = RandomString.defaultRandom) = new RandomFixedString(template, mask, random)
  }

}

