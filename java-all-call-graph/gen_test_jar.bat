rem ���Դ�������jar��
cmd /c gradlew test_gen_jar

rem �������ڱȽϵ�ʾ��jar��
cmd /c gradlew test_gen_diff_jar -Pexample_flag=1
cmd /c gradlew test_gen_diff_jar -Pexample_flag=2

rem �������ڱȽϵ�ʾ��jar����tar.gz��������һ����ͬ��jar��
cmd /c gradlew test_gen_diff_jar_with_same -Pexample_flag=1
cmd /c gradlew test_gen_diff_jar_with_same -Pexample_flag=2

rem �������ڱȽϵ�ʾ��jar����tar.gz��
cmd /c gradlew test_gen_diff_tar_gz -Pexample_flag=1
cmd /c gradlew test_gen_diff_tar_gz -Pexample_flag=2

rem ���ɰ���jar�ļ���jar�ļ�
cmd /c gradlew gen_run_jar
cmd /c gradlew gen_jar_in_jar

rem ���ɰ���jar�ļ���war�ļ�
cmd /c gradlew gen_jar_in_war
