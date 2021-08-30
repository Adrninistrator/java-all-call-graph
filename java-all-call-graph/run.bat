SETLOCAL ENABLEDELAYEDEXPANSION
set CLASSPATH=
FOR %%C IN (lib\*.jar) DO set CLASSPATH=!CLASSPATH!;%%C
echo %CLASSPATH%
java -cp .;./jar/run_jacg.jar;%CLASSPATH% test.jacg.TestRunnerWriteDb

pause...