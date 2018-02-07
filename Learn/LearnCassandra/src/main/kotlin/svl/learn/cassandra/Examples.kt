package svl.learn.cassandra

import com.datastax.driver.core.querybuilder.QueryBuilder.uuid
import svl.learn.cassandra.utils.CassandraConnector

fun main(args: Array<String>) {
    println("Hello, Cassandra")

    CassandraConnector("localhost").use{ connector ->
        simpleTable(connector)
        simpleTableWithTTL(connector)
    }
}

private fun printHeader(name:String) {
    println("================================== $name ==================================")
}

private fun simpleTable(connector: CassandraConnector) {
    printHeader("simpleTable")
    connector.createKeyspace("test")
    val session = connector.session

    listOf(
            "CREATE TABLE IF NOT EXISTS books (id uuid PRIMARY KEY, title text, publisher text, authors list<text>);",
            "INSERT INTO books(id, title, authors) values (${uuid()}, 'Cassandra handbook', ['John', 'Steve']);",
            "INSERT INTO books(id, title, publisher) values (${uuid()}, 'Another Cassandra book', 'apress');")
            .forEach { session.execute(it) }

    val rs = session.execute("select * from books;")
    rs.forEach { println(it) }
}

private fun simpleTableWithTTL(connector: CassandraConnector) {
    printHeader("simpleTableWithTTL")
    //TTL is time to live
    connector.createKeyspace("test")
    val session = connector.session

    listOf(
            "CREATE TABLE IF NOT EXISTS books (id int PRIMARY KEY, title text, publisher text, authors list<text>);",
            "INSERT INTO books(id, title, authors) values (1, 'Cassandra book', ['John', 'Steve']);",
            "INSERT INTO books(id, title, publisher) values (2, 'Another book', 'apress');",
            "UPDATE books USING TTL 3 SET publisher ='ME' WHERE id = 1;")
            .forEach { session.execute(it) }

    fun String?.format(chars: Int) =String.format("%-${chars}s", this)

    fun dumpResult() {
        val rs = session.execute("select id, title, publisher, authors from books;")
        val columns = rs.columnDefinitions
        rs.forEach {row ->
            columns.forEach{col ->
                print("${col.name}: ${(row.getObject(col.name)?.toString()?:"").format(20)} ")
            }
            println()
        }
    }

    dumpResult()
    Thread.sleep(3000)
    println("------------------------------------------------------------------------------")
    println("The publisher for the first book should be null after 3 seconds")
    dumpResult()
}

