rem ���Դ�������jar�ļ�
cmd /c gradlew test_gen_jar

rem �������ڱȽϵ�ʾ��jar�ļ�
cmd /c gradlew test_gen_diff_jar -Pexample_flag=1
cmd /c gradlew test_gen_diff_jar -Pexample_flag=2

rem �������ڱȽϵ�ʾ��jar�ļ���tar.gz��������һ����ͬ��jar�ļ�
cmd /c gradlew test_gen_diff_jar_with_same -Pexample_flag=1
cmd /c gradlew test_gen_diff_jar_with_same -Pexample_flag=2