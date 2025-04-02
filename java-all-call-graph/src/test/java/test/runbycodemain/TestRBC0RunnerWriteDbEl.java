package test.runbycodemain;

import com.adrninistrator.jacg.runner.RunnerWriteDb;
import com.adrninistrator.javacg2.conf.enums.JavaCG2ConfigKeyEnum;
import com.adrninistrator.javacg2.conf.enums.JavaCG2OtherConfigFileUseListEnum;
import com.adrninistrator.javacg2.el.enums.JavaCG2ElAllowedVariableEnum;
import com.adrninistrator.javacg2.el.enums.JavaCG2ElConfigEnum;
import org.junit.Assert;
import org.junit.Test;
import test.annotation.JACGExample;
import test.runbycode.base.TestRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2025/2/16
 * @description:
 */
@JACGExample(title = "解析代码并将结果写入数据库，使用表达式忽略特定内容",
        desc = {"需要先执行 unittest.gradle 中的命令，生成示例jar包： gradlew test_gen_jar",
                "通过代码指定配置参数的主要功能示例"})
public class TestRBC0RunnerWriteDbEl extends TestRunByCodeBase {

    @JACGExample(title = "所有的内容都不解析",
            desc = {"通过表达式实现"})
    @Test
    public void testElNone() {
        // java-callgraph2表达式开启调试
        javaCG2ConfigureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_EL_DEBUG_MODE, Boolean.TRUE.toString());
        for (JavaCG2ElConfigEnum javaCG2ElConfigEnum : JavaCG2ElConfigEnum.values()) {
            javaCG2ConfigureWrapper.setElConfigFixedTrue(javaCG2ElConfigEnum);
        }

        RunnerWriteDb runnerWriteDb = new RunnerWriteDb(javaCG2ConfigureWrapper, configureWrapper);
        // jar文件未发生变化时也强制重新解析后写入数据库
        runnerWriteDb.setSkipWhenNotModified(false);
        Assert.assertTrue(runnerWriteDb.run());
    }

    @JACGExample(title = "仅解析指定包下的类",
            desc = {"通过表达式实现，仅当类的包名以 test.callgraph.methodcall. 开头时不跳过"})
    @Test
    public void testElOnlyParseSomeClass() {
        // java-callgraph2表达式开启调试
        javaCG2ConfigureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_EL_DEBUG_MODE, Boolean.TRUE.toString());
        javaCG2ConfigureWrapper.setElConfigText(JavaCG2ElConfigEnum.ECE_PARSE_IGNORE_CLASS,
                "!string.startsWith(" + JavaCG2ElAllowedVariableEnum.EAVE_PARSE_PACKAGE_NAME.getVariableName() + ", 'test.callgraph.methodcall.')"
        );

        RunnerWriteDb runnerWriteDb = new RunnerWriteDb(javaCG2ConfigureWrapper, configureWrapper);
        // jar文件未发生变化时也强制重新解析后写入数据库
        runnerWriteDb.setSkipWhenNotModified(false);
        Assert.assertTrue(runnerWriteDb.run());
    }

    @JACGExample(title = "仅解析指定包下的类的方法调用",
            desc = {"通过表达式实现，仅当类的包名以 test.callgraph.methodcall. 开头时不跳过"})
    @Test
    public void testElOnlyParseSomeMethodCall() {
        // java-callgraph2表达式开启调试
        javaCG2ConfigureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_EL_DEBUG_MODE, Boolean.TRUE.toString());
        javaCG2ConfigureWrapper.setElConfigText(JavaCG2ElConfigEnum.ECE_PARSE_IGNORE_METHOD_CALL_ER,
                "!string.startsWith(" + JavaCG2ElAllowedVariableEnum.EAVE_MC_ER_PACKAGE_NAME.getVariableName() + ", 'test.callgraph.methodcall.')"
        );

        RunnerWriteDb runnerWriteDb = new RunnerWriteDb(javaCG2ConfigureWrapper, configureWrapper);
        // jar文件未发生变化时也强制重新解析后写入数据库
        runnerWriteDb.setSkipWhenNotModified(false);
        Assert.assertTrue(runnerWriteDb.run());
    }

    @JACGExample(title = "仅解析jar文件中指定路径下的jar文件",
            desc = {"通过表达式实现，当jar文件的目录名称为'lib'时跳过",
                    "需要先执行以下命令生成包含jar文件的jar文件",
                    "gradlew gen_run_jar",
                    "gradlew gen_jar_in_jar"})
    @Test
    public void testElOnlyParseSomeJarInJar() {
        javaCG2ConfigureWrapper.setOtherConfigList(JavaCG2OtherConfigFileUseListEnum.OCFULE_JAR_DIR, "build/jar_output_dir.jar");
        // java-callgraph2表达式开启调试
        javaCG2ConfigureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_EL_DEBUG_MODE, Boolean.TRUE.toString());
        javaCG2ConfigureWrapper.setElConfigText(JavaCG2ElConfigEnum.ECE_MERGE_FILE_IGNORE_JAR_IN_JAR_WAR,
                JavaCG2ElAllowedVariableEnum.EAVE_MF_FILE_DIR_PATH_IN_JAR_WAR.getVariableName() + "=='lib'"
        );

        RunnerWriteDb runnerWriteDb = new RunnerWriteDb(javaCG2ConfigureWrapper, configureWrapper);
        // jar文件未发生变化时也强制重新解析后写入数据库
        runnerWriteDb.setSkipWhenNotModified(false);
        Assert.assertTrue(runnerWriteDb.run());
    }

    @JACGExample(title = "仅解析war文件中指定路径下的jar文件",
            desc = {"通过表达式实现，当jar文件的目录名称为'WEB-INF/lib'时跳过",
                    "需要先执行以下命令生成包含jar文件的war文件",
                    "gradlew gen_run_jar",
                    "gradlew gen_jar_in_war"})
    @Test
    public void testElOnlyParseSomeJarInWar() {
        javaCG2ConfigureWrapper.setOtherConfigList(JavaCG2OtherConfigFileUseListEnum.OCFULE_JAR_DIR, "build/jar_output_dir.war");
        // java-callgraph2表达式开启调试
        javaCG2ConfigureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_EL_DEBUG_MODE, Boolean.TRUE.toString());
        javaCG2ConfigureWrapper.setElConfigText(JavaCG2ElConfigEnum.ECE_MERGE_FILE_IGNORE_JAR_IN_JAR_WAR,
                JavaCG2ElAllowedVariableEnum.EAVE_MF_FILE_DIR_PATH_IN_JAR_WAR.getVariableName() + "=='WEB-INF/lib'"
        );

        RunnerWriteDb runnerWriteDb = new RunnerWriteDb(javaCG2ConfigureWrapper, configureWrapper);
        // jar文件未发生变化时也强制重新解析后写入数据库
        runnerWriteDb.setSkipWhenNotModified(false);
        Assert.assertTrue(runnerWriteDb.run());
    }
}
