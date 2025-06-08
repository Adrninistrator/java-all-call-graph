rem 测试代码生成jar文件
cmd /c gradlew test_gen_jar

rem 生成用于比较的示例jar文件
cmd /c gradlew test_gen_diff_jar -Pexample_flag=1
cmd /c gradlew test_gen_diff_jar -Pexample_flag=2

rem 生成用于比较的示例jar文件的tar.gz包，包含一个相同的jar文件
cmd /c gradlew test_gen_diff_jar_with_same -Pexample_flag=1
cmd /c gradlew test_gen_diff_jar_with_same -Pexample_flag=2