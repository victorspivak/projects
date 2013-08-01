package svl.metadata.poc.md.database

import svl.metadata.poc.md.database.hbase.{DefaultHBaseDatabaseContext, HBaseDatabase}
import svl.metadata.poc.md.database.solr.DefaultSolrEnv

object MdDatabase {
  lazy val db = new HBaseDatabase
                    with DefaultHBaseDatabaseContext
                    with DefaultSolrEnv

  def apply() = db
}
