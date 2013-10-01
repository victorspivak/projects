package svl.learn.scala.classes

object Hierarchy {
	def main(args: Array[String]) {
		class BaseEntity(name:String)
		class ChildEntity(childName:String) extends BaseEntity(childName)

		trait IProcessor {
			def process(entity:BaseEntity)
		}

		class Processor extends IProcessor{
			def process(entity:BaseEntity) = println("Process 1")
			def process(entity:ChildEntity) = println("Process 2")
		}

		val processor:IProcessor = new Processor

		processor.process(new BaseEntity("Base"))
		processor.process(new ChildEntity("Child"))
	}
}
