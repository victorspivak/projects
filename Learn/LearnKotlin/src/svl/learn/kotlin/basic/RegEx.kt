package svl.learn.kotlin.basic

fun testRegex() {
    val mailRegex = """(.*?)@(.*)"""
    val mail = "victor.spivak@gmail.com"

    val result = Regex(mailRegex).matchEntire(mail)
    val name = result?.groupValues?.get(1)
    val domain = result?.groupValues?.get(2)

    print(name + "--->" + domain)
}