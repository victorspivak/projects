package svl.slick

import svl.slick.fs.FileSystemExample

object HelloSlick {

    def main(args: Array[String]) {
        
//        generate()
//        testDirectEmbedded()
//        testLiftedEmbedded()

        FileSystemExample.run()
    }

    def generate() = scala.slick.model.codegen.SourceCodeGenerator.main(
        Array(Settings.slickDriver, Settings.driver, Settings.dbUrl,
            Settings.outputGenDir, Settings.genSchemaPackage,
            Settings.user, Settings.password)
    )

    def testDirectEmbedded() {
        println("Hello Slick - Direct Embedded flavor")
        new DirectEmbeddingSample()
    }

    def testLiftedEmbedded() {
        println("Hello Slick - Lifted Embedded flavor")
        new LiftedEmbeddingSample()
    }
}
