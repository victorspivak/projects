package svl.interviews.simpledb

import org.junit.Assert._
import org.junit.Test


@Test
class TestDatabase {
    @Test
    def testBasicOperations() {
        val db = new MemoryDatabase[String, String] with DatabaseTransaction[String, String]

        db.set("k1", "v1")
        val v1 = db.get("k1")
        val v2 = db.get("k2")

        v1 match {
            case Some(v) => assertEquals("v1", v)
            case None => assertFalse(true)
        }

        v2.map{v=>assertFalse(true)}

        val oldValue = db.set("k1", "v11")
        oldValue match {
            case Some(v) => assertEquals("v1", v)
            case None => assertFalse(true)
        }

        val v11 = db.get("k1")

        v11 match {
            case Some(v) => assertEquals("v11", v)
            case None => assertFalse(true)
        }
    }

    @Test
    def testDelete() {
        val db = new MemoryDatabase[String, String] with DatabaseTransaction[String, String]

        db.set("k1", "v1")
        val v1 = db.get("k1")

        v1 match {
            case Some(v) => assertEquals("v1", v)
            case None => assertFalse(true)
        }

        val oldValue = db.delete("k1")

        oldValue match {
            case Some(v) => assertEquals("v1", v)
            case None => assertFalse(true)
        }

        db.get("k1") match {
            case Some(v) => assertFalse(true)
            case None => assertFalse(false)
        }
    }

    @Test
    def testCount() {
        val db = new MemoryDatabase[String, String] with DatabaseTransaction[String, String]

        db.set("k1", "v1")
        db.set("k2", "v1")
        db.set("k3", "v2")

        assertEquals(2, db.count("v1"))
        assertEquals(1, db.count("v2"))

        db.delete("k1")
        db.delete("k3")

        assertEquals(1, db.count("v1"))
        assertEquals(0, db.count("v2"))
    }

    @Test
    def testSimpleTransaction() {
        val db = new MemoryDatabase[String, String] with DatabaseTransaction[String, String]

        db.beginTrans()
        db.set("k1", "v1")
        db.set("k2", "v1")
        db.set("k3", "v2")

        assertEquals(2, db.count("v1"))
        assertEquals(1, db.count("v2"))

        db.delete("k1")
        db.delete("k3")

        assertEquals(1, db.count("v1"))
        assertEquals(0, db.count("v2"))
        db.commit()

        assertEquals(1, db.count("v1"))
        assertEquals(0, db.count("v2"))
    }

    @Test
    def testRollback() {
        val db = new MemoryDatabase[String, String] with DatabaseTransaction[String, String]

        db.set("k1", "v1")
        db.set("k2", "v1")
        db.set("k3", "v2")

        assertEquals(2, db.count("v1"))
        assertEquals(1, db.count("v2"))

        db.beginTrans()
        db.delete("k1")
        db.delete("k3")

        assertEquals(1, db.count("v1"))
        assertEquals(0, db.count("v2"))
        db.rollback()

        assertEquals(2, db.count("v1"))
        assertEquals(1, db.count("v2"))
    }
}
