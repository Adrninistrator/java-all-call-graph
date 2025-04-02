rem 测试代码生成jar包
cmd /c gradlew test_gen_jar

rem 生成用于比较的示例jar包
cmd /c gradlew test_gen_diff_jar -Pexample_flag=1
cmd /c gradlew test_gen_diff_jar -Pexample_flag=2

rem 生成用于比较的示例jar包的tar.gz包，包含一个相同的jar包
cmd /c gradlew test_gen_diff_jar_with_same -Pexample_flag=1
cmd /c gradlew test_gen_diff_jar_with_same -Pexample_flag=2

rem 生成用于比较的示例jar包的tar.gz包
cmd /c gradlew test_gen_diff_tar_gz -Pexample_flag=1
cmd /c gradlew test_gen_diff_tar_gz -Pexample_flag=2

rem 生成包含jar文件的jar文件
cmd /c gradlew gen_run_jar
cmd /c gradlew gen_jar_in_jar

rem 生成包含jar文件的war文件
cmd /c gradlew gen_jar_in_war
