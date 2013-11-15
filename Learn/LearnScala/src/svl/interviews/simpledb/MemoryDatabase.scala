package svl.interviews.simpledb

import scala.collection.mutable

object Operation extends Enumeration {

    type Operation = Value
    val SET, DELETE = Value
}
import Operation._

class MemoryDatabase[K,V] extends Database[K,V] with Transaction{
    val keyValueIndex = new scala.collection.mutable.HashMap[K, V]
    val valueKeyIndex = new scala.collection.mutable.HashMap[V, scala.collection.mutable.Set[K]]
    val transactions = new mutable.Stack[TransactionState[K,V]]
    var currentTransaction:Option[TransactionState[K,V]] = None

    def set(key: K, value: V) = {
        setImp(key, value)(useTransaction = true)
    }

    private def setImp(key: K, value: V)(useTransaction:Boolean) = {
        val oldValue = keyValueIndex.get(key)

        removeFromValueIndex(key, oldValue)

        keyValueIndex.put(key, value)
        valueKeyIndex.get(value).map{keys=>
            keys.add(key)
        }.orElse(valueKeyIndex.put(value, scala.collection.mutable.Set(key)))

        if(useTransaction) {
            currentTransaction.map{transaction=>
                transaction.addOp(SET, key, oldValue)
            }
        }

        oldValue
    }

    def get(key: K) = {
        keyValueIndex.get(key).map{v=>
            println(v)
            v
        }.orElse{
            println("NULL")
            None
        }
    }

    def delete(key: K):Option[V] = {
        deleteImp(key)(useTransaction = true)
    }

    private def deleteImp(key: K)(useTransaction:Boolean) = {
        val oldValue = keyValueIndex.get(key)

        removeFromValueIndex(key, oldValue)

        if (useTransaction){
            currentTransaction.map{transaction=>
                transaction.addOp(DELETE, key, oldValue)
            }
        }

        oldValue
    }

    def count(value: V) = {
        val num = valueKeyIndex.get(value).map(_.size).getOrElse(0)
        println(num)
        num
    }

    def beginTrans() = {
        val transaction = new TransactionState[K,V]
        currentTransaction.map(transactions.push)
        currentTransaction = Some(transaction)
    }

    def commit() = {
        currentTransaction = None
        transactions.clear()
    }

    def rollback() = {
        currentTransaction match {
            case Some(transaction) =>
                undo(transaction.ops)
                if (!transactions.isEmpty){
                    currentTransaction = Some(transactions.pop())
                }

            case None => println("No Transaction")
        }
    }

    private def removeFromValueIndex(key:K, oldValue:Option[V]) {
        oldValue.map{value=>
            valueKeyIndex.get(value).map(_.remove(key))
        }
    }

    private def undo(ops:mutable.Stack[UndoOperation[K,V]]){
        ops match {
            case mutable.Stack() =>
            case _ =>
                undo(ops.pop())
                undo(ops)
        }
    }

    private def undo(op:UndoOperation[K,V]){
        op match {
            case UndoOperation(SET, key, oldValue) =>
                oldValue match {
                    case Some(v) => setImp(key, v)(useTransaction = false)
                    case None => deleteImp(key)(useTransaction = false)
                }
            case UndoOperation(DELETE, key, oldValue) =>
                oldValue match {
                    case Some(v) => setImp(key, v)(useTransaction = false)
                }
        }
    }
}

case class UndoOperation[K, V](op:Operation, key:K, oldValue:Option[V])

class TransactionState[K,V]{
    val ops = new scala.collection.mutable.Stack[UndoOperation[K,V]]

    def addOp(op:Operation, key:K, oldValue:Option[V]){
        ops.push(UndoOperation(op, key, oldValue))
    }
}