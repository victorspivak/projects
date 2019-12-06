package svl.learn.kotlin.dsl

import svl.learn.kotlin.dsl.PersonDslExample.person

fun main(args: Array<String>) {
    testPerson()
    testPipeline()
}

private fun testPerson() {
    val person = person {
        name = "John"
        dateOfBirth = "1980-12-01"
        addresses {
            address {
                country = "England"
                street = "Main Street"
                number = 12
                city = "London"
            }
            usa {
                street = "Market str"
                number = 42
                city = "San Francisco"
            }
        }
    }
    println(person)
}

private fun testPipeline() {
    val pipeline = pipeline {
        name = "Extract email"
        operations {
            filter {
                name = "Filter spam"
                source = "inbox"
                dest = "no-spam"
            }
        }
    }

    println(pipeline)
}
