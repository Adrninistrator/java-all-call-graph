rem 测试代码生成jar文件等
cmd /c gen_diff_jar.bat

rem 生成包含jar文件的jar文件
cmd /c gradlew gen_run_jar
cmd /c gradlew gen_jar_in_jar

rem 生成包含jar文件的war文件
cmd /c gradlew gen_jar_in_war
