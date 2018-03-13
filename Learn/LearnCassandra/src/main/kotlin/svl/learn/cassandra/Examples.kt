package svl.learn.cassandra

import com.datastax.driver.core.Session
import com.datastax.driver.core.querybuilder.QueryBuilder.uuid
import com.datastax.driver.mapping.annotations.Column
import com.datastax.driver.mapping.annotations.PartitionKey
import com.datastax.driver.mapping.annotations.Table
import svl.learn.cassandra.utils.CassandraConnector
import com.datastax.driver.mapping.MappingManager


fun main(args: Array<String>) {
    println("Hello, Cassandra")

    CassandraConnector("localhost").use{ connector ->
//        simpleTable(connector)
//        simpleTableWithTTL(connector)
//        materializedView(connector)
        mapperExample(connector)
    }
}

private fun printHeader(name:String) {
    println("================================== $name ==================================")
}

fun String?.format(chars: Int) =String.format("%-${chars}s", this)

fun dumpResult(query:String, session: Session) {
    println("***** Result for '$query' *****")
    val rs = session.execute(query)
    val columns = rs.columnDefinitions
    rs.forEach {row ->
        columns.forEach{col ->
            print("${col.name}: ${(row.getObject(col.name)?.toString()?:"").format(20)} ")
        }
        println()
    }
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

    dumpResult("select * from books;", session)
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

    val query = "select id, title, publisher, authors from books;"

    dumpResult(query, session)
    Thread.sleep(3000)
    println("------------------------------------------------------------------------------")
    println("The publisher for the first book should be null after 3 seconds")
    dumpResult(query, session)
}

private fun materializedView(connector: CassandraConnector) {
    printHeader("materializedView")
    connector.createKeyspace("test")
    val session = connector.session

    listOf(
            "CREATE TABLE IF NOT EXISTS books (book_id int, title text, publisher text, authors list<text>, PRIMARY KEY(book_id));",
            "CREATE MATERIALIZED VIEW books_by_publisher AS SELECT * FROM books WHERE publisher IS NOT NULL PRIMARY KEY(publisher, book_id);",
            "INSERT INTO books(book_id, title, authors, publisher) values (1, 'Cassandra book', ['John', 'Steve'], 'Manning');",
            "INSERT INTO books(book_id, title, publisher) values (2, 'Another book', 'apress');",
            "INSERT INTO books(book_id, title, authors, publisher) values (11, 'Java 8', ['Gasling'], 'Manning');",
            "INSERT INTO books(book_id, title, authors, publisher) values (11, 'Java 9', ['Gasling'], 'apress');"
            ).forEach { session.execute(it) }

    dumpResult("select * from books;", session)
    dumpResult("select * from books_by_publisher;", session)
    dumpResult("select title from books_by_publisher where publisher='apress';", session)
}

private fun mapperExample(connector: CassandraConnector) {
    printHeader("mapperExample")
    connector.createKeyspace("test")
    val session = connector.session

    listOf(
            "CREATE TABLE IF NOT EXISTS books (book_id int, title text, publisher text, authors list<text>, PRIMARY KEY(book_id));",
            "CREATE MATERIALIZED VIEW books_by_publisher AS SELECT publisher, book_id, title FROM books WHERE publisher IS NOT NULL PRIMARY KEY(publisher, book_id);"
            ).forEach { session.execute(it) }

    @Table(keyspace = "test", name = "books")
    class Book(bookId_:Int, title_:String, publisher_:String, authors_:List<String>) {
        @PartitionKey
        @Column(name = "book_id") var bookId = bookId_
        @Column(name = "title") var title = title_
        @Column(name = "publisher") var publisher = publisher_
        @Column(name = "authors") var authors = authors_

        constructor() : this(0, "", "", listOf())
        override fun toString(): String {
            return "Book(book_id=$bookId, title='$title', publisher='$publisher', authors=$authors)"
        }
    }

    @Table(keyspace = "test", name = "books_by_publisher")
    class BooksByPublisher(bookId_:Int, title_:String, publisher_:String) {
        @PartitionKey
        @Column(name = "publisher") var publisher = publisher_
        @Column(name = "book_id") var bookId = bookId_
        @Column(name = "title") var title = title_

        constructor() : this(0, "", "")
        override fun toString(): String {
            return "BooksByPublisher(book_id=$bookId, title='$title', publisher='$publisher')"
        }
    }

    val mappingManager = MappingManager(session)
    val bookMapper = mappingManager.mapper(Book::class.java)

    bookMapper.save(Book(1, "Java", "Manning", listOf("Gasling")))
    bookMapper.save(Book(2, "Java 8", "apress", listOf("John", "Oracle")))
    bookMapper.save(Book(3, "Kotlin", "apress", listOf("Peter")))

    dumpResult("select * from books;", session)

    println("book = ${bookMapper.get(1)}")
    println("book = ${bookMapper.get(2)}")
    println("book = ${bookMapper.get(3)}")

    dumpResult("select * from books_by_publisher;", session)

    val booksByPublisherMapper = mappingManager.mapper(BooksByPublisher::class.java)
    println("BooksByPublisher = ${booksByPublisherMapper.get("Manning")}")
    println("BooksByPublisher = ${booksByPublisherMapper.get("apress")}")

    //below we using mapper to get statement and then to map result set to an entity object
    booksByPublisherMapper.map(session.execute(booksByPublisherMapper.getQuery("apress")))
            .forEach(::println)
}
