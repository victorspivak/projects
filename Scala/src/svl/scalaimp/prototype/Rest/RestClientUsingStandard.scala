import io.Source
import org.apache.http.client.methods.HttpGet
import org.apache.http.impl.client.DefaultHttpClient
import reflect.BeanInfo
import sjson.json.Serializer.SJSON
import util.parsing.json.JSON

object RestClientUsingStandard {
    def main (args:Array[String]) {
        @BeanInfo case class User(name: String, description: String) {}
        @BeanInfo case class Users(users:List[User]) {def this() = this(List())}

        class CC[T] { def unapply(a:Any):Option[T] = Some(a.asInstanceOf[T]) }

        object M extends CC[Map[String, Any]]
        object L extends CC[List[Any]]
        object S extends CC[String]
        object D extends CC[Double]
        object B extends CC[Boolean]

        val url = "http://api.twitter.com/1/users/show.json?screen_name=aloiscochard"
        val httpClient = new DefaultHttpClient
        val getReq = new HttpGet(url)
        val response = httpClient.execute(getReq)

        val entity = response.getEntity

        //for {
        //    Some(M(map)) <- List(JSON.parseFull(json))
        //    L(languages) = map("languages")
        //    M(language) <- languages
        //    S(name) = language("name")
        //    B(active) = language("is_active")
        //    D(completeness) = language("completeness")
        //} yield {
        //    (name, active, completeness)
        //}


        if (entity != null) {
            val json = Source.fromInputStream(entity.getContent).getLines().mkString
            println(json)

            val res = for {
                Some(M(user)) <- List(JSON.parseFull(json))
                S(name) = user("name")
                S(description) = user("description")
            } yield {
                new User(name, description)
            }

            println(res)
        }

        val users = new Users(List(new User("vic spivak", "It is me"), new User("vad spivak", "it is my son")))

        val out = SJSON.toJSON(users)

        println(out)

        val res1 = for {
            Some(M(map)) <- List(JSON.parseFull(out))
            L(users) = map("users")
            M(user) <- users
            S(name) = user("name")
            S(description) = user("description")
        } yield {
            new User(name, description)
        }

        println(res1)

        val res2 = for {
            Some(M(map)) <- List(JSON.parseFull(out))
            L(users_) = map("users")
            users = for {
                M(user) <- users_
                S(name) = user("name")
                S(description) = user("description")
            } yield {
                new User(name, description)
            }
        } yield {
            new Users(users)
        }

        println(res2.head)
    }
}




