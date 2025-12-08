rem 生成包含jar文件的jar文件
cmd /c gradlew gen_run_jar
cmd /c gradlew gen_jar_in_jar

rem 生成包含jar文件的war文件
cmd /c gradlew gen_jar_in_war
