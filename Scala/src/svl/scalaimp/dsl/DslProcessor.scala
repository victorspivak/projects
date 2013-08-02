package svl.scalaimp.dsl

import collection.mutable.ArrayBuffer

trait DslCommand {
    private var executed = false
    def executeIfNeeded() {
        if (!executed) {
            executed = true
            execute()
        }
    }

    def execute()
}

object DslProcessorMode extends Enumeration {
    type DslProcessorMode = Value
    val Immediate, AtOnce = Value
}
import DslProcessorMode._

object DslProcessor {
    val _staging = new ThreadLocal[ArrayBuffer[DslCommand]]
    def init() {_staging.set(new ArrayBuffer[DslCommand]())}
    def staging = _staging.get
    def reset() {_staging.remove()}

    def remember[T <: DslCommand](command: T) (implicit mode:DslProcessorMode = Immediate) = {
        if (mode == Immediate) {
            val commands = staging
            if (commands.size > 0)
                commands.last.executeIfNeeded()
        }

        staging.append(command)
        command
    }

    def process(body: => Unit) (implicit mode:DslProcessorMode = Immediate) = {
        DslProcessor.init()
        body
        val commands = DslProcessor.staging
        DslProcessor.reset()
        if (mode == AtOnce)
            commands.foreach(_.executeIfNeeded())
        commands.toList
    }
}

