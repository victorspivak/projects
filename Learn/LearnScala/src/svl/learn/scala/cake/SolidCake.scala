package svl.learn.scala.cake

object App {
    def main(args:Array[String]) {
		val cprPersister = new ZKCheckpointPersistence with LiftJSONCheckpointSerialization

		val cpr = CheckpointRecord("CP 1")
		cprPersister.checkpointRepository.recordCheckpoint(cpr, "vspivak-h")
		val cpr1 = cprPersister.checkpointRepository.restoreCheckpoint("CP 1")
		println(cpr1)
    }
}

case class CheckpointRecord(id:String)

/** Component obligation for Checkpoint storage and retrieval */
trait CheckpointPersistence { // ⬅ The Component Obligation ⬇
  def checkpointRepository: CheckpointRepository // ⬅ The Provider

  trait CheckpointRepository { // ⬅ The Characteristic ⬇
    def recordCheckpoint(cpr: CheckpointRecord, hostName: String)
    def restoreCheckpoint(cprPath: String): CheckpointRecord
    def deleteCheckpoint(cprPath: String)
  }
}

/** Zookeeper-based Checkpoint persistence component realization */
trait   ZKCheckpointPersistence // ⬅ A Component Realization ⬇
extends CheckpointPersistence {
	serializator: CheckpointSerialization => // ⬅ Component Dependency

	val checkpointRepository = new ZKCheckpointRepository // ⬅ Component Provider

	class   ZKCheckpointRepository // ⬅ The Characteristic Implementation ⬇
	extends CheckpointRepository {
		def recordCheckpoint(cpr: CheckpointRecord, hostName: String) {
			printf ("recordCheckpoint %s %s\n", cpr.toString, hostName)
			serializator.serialize(cpr)
		}

		/** Reconstitute a CheckpointRecord from the persistence store */
		def restoreCheckpoint(cprPath: String): CheckpointRecord = {
			printf ("restoreCheckpoint %s\n", cprPath)
			serializator.deserialize(cprPath)
		}

		def deleteCheckpoint(path: String) {
			printf ("deleteCheckpoint %s\n", path)
		}
	}
}

trait CheckpointSerialization {
	def serialize(cpr:CheckpointRecord):String
	def deserialize(image:String):CheckpointRecord
}

trait LiftJSONCheckpointSerialization extends CheckpointSerialization {
	def serialize(cpr: CheckpointRecord): String = {
		printf ("LiftJSONCheckpointSerialization.serialize %s\n", cpr)
		cpr.id
	}

	def deserialize(image: String): CheckpointRecord = {
		printf ("LiftJSONCheckpointSerialization.deserialize %s\n", image)
		CheckpointRecord(image)
	}
}
