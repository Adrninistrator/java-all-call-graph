rem ���Դ�������jar�ļ���
cmd /c gen_diff_jar.bat

rem ���ɰ���jar�ļ���jar�ļ�
cmd /c gradlew gen_run_jar
cmd /c gradlew gen_jar_in_jar

rem ���ɰ���jar�ļ���war�ļ�
cmd /c gradlew gen_jar_in_war
