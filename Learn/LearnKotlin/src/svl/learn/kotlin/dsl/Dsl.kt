package svl.learn.kotlin.dsl

import java.text.SimpleDateFormat
import svl.learn.kotlin.dsl.PersonExample.Person;
import svl.learn.kotlin.dsl.PersonExample.Address;
import svl.learn.kotlin.dsl.PersonExample.AddressImpl;
import svl.learn.kotlin.dsl.PersonExample.UsaAddress;
import java.util.*
import kotlin.collections.ArrayList

object PersonDslExample {
    @DslMarker annotation class PersonDsl

    fun person(block: PersonBuilder.() -> Unit): Person = PersonBuilder().apply(block).build()

    @PersonDsl class PersonBuilder {
        var name: String = ""
        private var dob: Date = Date()
        var dateOfBirth: String = ""
            set(value) {
                dob = SimpleDateFormat("yyyy-MM-dd").parse(value)
            }

        private val addresses = mutableListOf<Address>()

        fun addresses(block: ADDRESSES.() -> Unit) {
            addresses.addAll(ADDRESSES().apply(block))
        }

        fun build(): Person = Person(name, dob, addresses)
    }

    @PersonDsl class ADDRESSES : ArrayList<Address>() {
        fun address(block: AddressBuilder.() -> Unit) {
            add(AddressBuilder().apply(block).build())
        }
        fun usa(block: UsaAddressBuilder.() -> Unit) {
            add(UsaAddressBuilder().apply(block).build())
        }
    }

    @PersonDsl class AddressBuilder {
        var country: String = ""
        var street: String = ""
        var number: Int = 0
        var city: String = ""

        fun build(): Address = AddressImpl(country, street, number, city)
    }

    @PersonDsl class UsaAddressBuilder {
        var street: String = ""
        var number: Int = 0
        var city: String = ""

        fun build(): Address = UsaAddress(street, number, city)
    }
}

@DslMarker annotation class PipelineDsl

fun pipeline(block: PipelineBuilder.() -> Unit): Pipeline = PipelineBuilder().apply(block).build()

@PipelineDsl class PipelineBuilder {
    var name: String = ""
    private val operations = mutableListOf<Operation<*>>()

    fun operations(block: OPERATIONS.() -> Unit) {
        operations.addAll(OPERATIONS().apply(block))
    }

    fun build(): Pipeline = Pipeline(name, operations)
}

@PipelineDsl class OPERATIONS : ArrayList<Operation<*>>() {
    fun filter(block: FilterBuilder.() -> Unit) {
        add(FilterBuilder().apply(block).build())
    }
}

@PipelineDsl class FilterBuilder {
    var name: String = ""
    var source: String = ""
    var dest: String = ""

    fun build(): Operation<*> = Filter<String>(name, source, dest)
}
