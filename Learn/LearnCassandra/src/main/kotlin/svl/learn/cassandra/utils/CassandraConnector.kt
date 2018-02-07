package svl.learn.cassandra.utils

import com.datastax.driver.core.Cluster
import com.datastax.driver.core.Session

class CassandraConnector(host:String, port:Int = 9042) : AutoCloseable{
    private val cluster:Cluster
    val session: Session

    init {
        cluster = Cluster.builder()
                .addContactPoint(host)
                .withPort(port)
                .build()

        session = cluster.connect()
    }

    fun isKeyspaceExists(name:String):Boolean {
        val rs = cluster.metadata.keyspaces
        return rs
                .find { it.name == name } != null
    }

    fun createKeyspace(name:String, replicationFactor:Int = 1, replicationStrategy:String = "SimpleStrategy") {
        session.execute("use system;")
        if (isKeyspaceExists(name))
            session.execute("drop keyspace $name;")
        session.execute("create keyspace $name with replication = {'class' : '$replicationStrategy', 'replication_factor' : $replicationFactor };")
        session.execute("use $name;")
    }

    override fun close() {
        session.close()
        cluster.close()
    }
}