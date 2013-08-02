import svl.sfdc.client.dsl.SfdcDsl._

val script = Script({
    login as "v@test.com" password "123456" to "localhost:8080"
    query records "select count from ContentDocument"
    describe entity "ContentDocument"
    describe entity "ContentDocument" detail true
    describe.global
    val rec1 = create record "ContentDocument" where ("Title" -> "My title", "OwnerId" -> "0000000")
    create record "ContentDocument" where ("Title" -> "My title", "OwnerId" -> "0000000", "Assoc" -> rec1.id)

    (1 to 5).foreach({i =>
        create record "ContentDocument" where ("Title" -> "My title %d".format(i), "OwnerId" -> "0000000%d".format(i), "Assoc" -> rec1.id)
    })
})
