package svl.learn.kotlin.dsl

import java.util.*

object PersonExample{
    interface Address {
        val country: String
        val street: String
        val number: Int
        val city: String
    }

    data class AddressImpl(override val country: String,
                           override val street: String,
                           override val number: Int,
                           override val city: String) : Address

    data class UsaAddress(override val street: String,
                          override val number: Int,
                          override val city: String) : Address {
        override val country: String
            get() = "USA"
    }

    data class Person(val name: String,
                      val dateOfBirth: Date,
                      var addresses: List<Address>)
}

//=================================================================================================================
interface Operation<I> {
    val name:String
    val sourceName:String
}

data class Filter<I>(override val name:String,
                     override val sourceName:String,
                     val destName:String): Operation<I>

data class Transform<I, O>(override val name:String,
                           override val sourceName:String,
                           val destName:String): Operation<I>

data class Pipeline(val name: String, val operations:List<Operation<*>>)

