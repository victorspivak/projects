package svl.interviews.simpledb

import scala.collection.mutable

trait DatabaseTransaction[K,V] extends Database[K,V] with Transaction{
    this:Database[K,V]=>

    val transactions = new mutable.Stack[TransactionState[K,V]]
    var currentTransaction:Option[TransactionState[K,V]] = None

    override abstract def set(key:K, value:V) = {
        val oldValue = dbSet(key, value)
        currentTransaction.map{transaction=>
            transaction.addOp(UndoSet(key, oldValue))
        }

        oldValue
    }

    override abstract def delete(key:K) = {
        val oldValue = dbDelete(key)
        currentTransaction.map{transaction=>
            transaction.addOp(UndoDelete(key, oldValue))
        }

        oldValue
    }

    def dbSet(key:K, value:V) = {
        super.set(key, value)
    }

    def dbDelete(key:K) = {
        super.delete(key)
    }

    def beginTrans() = {
        val transaction = new TransactionState[K,V]
        currentTransaction.map(transactions.push)
        currentTransaction = Some(transaction)
    }

    def commit() = {
        currentTransaction match {
            case Some(_) =>
                currentTransaction = None
                transactions.clear()

            case None => throw new NoTransactionException
        }
    }

    def rollback() = {
        currentTransaction match {
            case Some(transaction) =>
                undo(transaction.ops)
                if (!transactions.isEmpty)
                    currentTransaction = Some(transactions.pop())
                else
                    currentTransaction = None

            case None => throw new NoTransactionException
        }
    }


    private def undo(ops:mutable.Stack[UndoOperation[K,V]]){
        ops match {
            case mutable.Stack() =>
            case _ =>
                ops.pop().undo(this)
                undo(ops)
        }
    }
}


trait UndoOperation[K,V]{
    def undo(transaction: DatabaseTransaction[K, V])
}

case class UndoSet[K, V](key:K, oldValue:Option[V]) extends UndoOperation[K,V]{
    def undo(transaction: DatabaseTransaction[K, V]) {
        oldValue match {
            case Some(v) => transaction.dbSet(key, v)
            case None => transaction.dbDelete(key)
        }
    }
}

case class UndoDelete[K, V](key:K, oldValue:Option[V]) extends UndoOperation[K,V]{
    def undo(transaction: DatabaseTransaction[K, V]) {
        oldValue match {
            case Some(v) => transaction.dbSet(key, v)
            case None =>
        }
    }
}

class TransactionState[K,V]{
    val ops = new scala.collection.mutable.Stack[UndoOperation[K,V]]

    def addOp(op:UndoOperation[K,V]){
        ops.push(op)
    }
}
