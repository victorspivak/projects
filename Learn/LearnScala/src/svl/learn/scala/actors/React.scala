import actors.Actor
import actors.Actor._
import java.net.{InetAddress, UnknownHostException}

object NameResolver extends Actor {
    def act() {
        react {
            case (name: String, actor: Actor) =>
                actor ! getIp(name)
                act()
            case "EXIT" =>
                println("Name resolver exiting.")
            case msg =>
                println("Unhandled message: "+ msg)
                act()
        }
    }
    def getIp(name: String): Option[InetAddress] = {
        try {
            Some(InetAddress.getByName(name))
        } catch {
            case _:UnknownHostException => None
        }
    }
}

NameResolver.start()

NameResolver ! ("www.microsoft.com", self)
self.receiveWithin(10000) { case x => println(x) }
NameResolver ! ("www.google.com", self)
self.receiveWithin(10000) { case x => println(x) }
NameResolver ! ("wwwwww.microsoft.com", self)
self.receiveWithin(10000) { case x => println(x) }
NameResolver ! ("www.microsoft.com", NameResolver)
NameResolver ! ("wwwwww.microsoft.com")
NameResolver ! ("EXIT")


object NameResolver1 extends Actor {
    def act() {
        loop {
            react {
                case (name: String, actor: Actor) =>
                    actor ! getIp(name)
                case "EXIT" =>
                    println("Name resolver exiting.")
                    exit()
                case msg =>
                    println("Unhandled message: "+ msg)
            }
        }
    }
    def getIp(name: String): Option[InetAddress] = {
        try {
            Some(InetAddress.getByName(name))
        } catch {
            case _:UnknownHostException => None
        }
    }
}

NameResolver1.start()

NameResolver1 ! ("www.microsoft.com", self)
self.receiveWithin(10000) { case x => println(x) }
NameResolver1 ! ("www.google.com", self)
self.receiveWithin(10000) { case x => println(x) }
NameResolver1 ! ("wwwwww.microsoft.com", self)
self.receiveWithin(10000) { case x => println(x) }
NameResolver1 ! ("wwwwww.microsoft.com")
NameResolver1 ! ("EXIT")
NameResolver1 ! ("EXIT")
