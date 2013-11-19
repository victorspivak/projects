package svl.interviews.simpledb

trait Database[K, V] {
    def set(key:K, value:V):Option[V]
    def get(key:K):Option[V]
    def delete(key:K):Option[V]
    def count(value:V):Int
}

trait Transaction{
    def beginTrans()
    def commit()
    def rollback()
}

