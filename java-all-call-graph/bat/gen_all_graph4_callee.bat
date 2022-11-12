SETLOCAL ENABLEDELAYEDEXPANSION
set CLASSPATH=
FOR %%C IN (lib\*.jar) DO set CLASSPATH=!CLASSPATH!;%%C
echo %CLASSPATH%
java -Dfile.encoding=UTF-8 -cp .;./config;./jar/run_jacg.jar;%CLASSPATH% test.jacg.TestRunnerGenAllGraph4Callee

pause...