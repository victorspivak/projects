package svl.sfdc.client.dsl

import svl.scalaimp.defs._Defs_._
import svl.scalaimp.dsl.{DslProcessorMode, DslProcessor, DslCommand}
import util.Random

object SfdcDsl {
    implicit val mode = DslProcessorMode.Immediate

    class Script(val params: List[DslCommand]) {
        def this(opts: Array[DslCommand]) = this(opts.toList)

        override def toString = params.foldLeft("Commands:\n") {
            (buf, entry) => buf + "\t" + entry + "\n"
        }
    }

    object Script {
        def apply(body: => Unit) = {
            new Script(DslProcessor.process(body))
        }

        def apply() = new Script(List())
    }

    case class login (user:String) extends DslCommand {
        private var _passoword: Option[String] = None
        private var _address: Option[String] = None

        def password(value: String) = {_passoword = value; this}
        def to(value: String) = {_address = value; this}

        def address = option2String(_address)
        def password = option2String(_passoword)

        def execute() {println("Login as %s with %s password to %s org".format(user, password, address))}
    }

    object login {
        def as(user:String) = DslProcessor.remember(new login(user))
    }

    case class query (sql:String) extends DslCommand {
        def execute() {println("Query %s".format(sql))}
    }

    object query {
        def records(sql:String) = DslProcessor.remember(new query(sql))
    }

    case class describe (name:String) extends DslCommand {
        private var _detail = false

        def detail(value: Boolean) = {_detail = value; this}
        def detail = _detail

        def execute() {println("describe %s with detail %s".format(name, detail))}
    }

    class describeGlobal extends DslCommand {
        private var _detail = false

        def detail(value: Boolean) = {_detail = value; this}
        def detail = _detail

        def execute() {println("describe global with detail %s".format(detail))}
    }

    object describe {
        def entity(name:String) = DslProcessor.remember(new describe(name))
        def global = DslProcessor.remember(new describeGlobal)
    }

    case class create (name:String) extends DslCommand {
        private var _props: Option[List[(String, String)]] = None
        private var _id = ""

        def where (value : (String, String)*) = {_props = Some(value.toList);this}
        def props = _props
        def id = {executeIfNeeded();_id}

        def execute() {
            println("create %s record with %s properties".format(name, props.toList))
            _id = "id_" + create.rand.nextInt(10000)
        }
    }

    object create {
        val rand = new Random //svl to remove
        def record(name:String) = DslProcessor.remember(new create(name))
    }
}
