out=$3${2//\//-}.txt
echo "Check $out for output"
java -classpath /home/victor/dev/projects/Tools/Scala/LendingClub/out/production/LendingClub:/home/victor/.ivy2/cache/org.scala-lang/scala-library/jars/scala-library-2.11.0.jar:/home/victor/.ivy2/cache/org.scala-lang/scala-reflect/jars/scala-reflect-2.11.0.jar svl.lendingclub.Analyser $1 $2 >$out
