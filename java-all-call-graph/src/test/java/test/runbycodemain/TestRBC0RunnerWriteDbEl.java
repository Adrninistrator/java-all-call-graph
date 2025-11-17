package test.runbycodemain;

import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.runner.RunnerWriteDb;
import com.adrninistrator.javacg2.conf.JavaCG2ConfigureWrapper;
import com.adrninistrator.javacg2.conf.enums.JavaCG2ConfigKeyEnum;
import com.adrninistrator.javacg2.conf.enums.JavaCG2OtherConfigFileUseListEnum;
import com.adrninistrator.javacg2.el.enums.CommonElAllowedVariableEnum;
import com.adrninistrator.javacg2.el.enums.JavaCG2ElAllowedVariableEnum;
import com.adrninistrator.javacg2.el.enums.JavaCG2ElConfigEnum;
import org.junit.Assert;
import org.junit.Test;
import test.annotation.JACGExample;
import test.runbycode.base.TestRunByCodeBase;
import test.runbycode.config.TestConfigGenerator;
import test.runbycode.util.JACGTestUtil;

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
    public void testElFixedTrueParseNone() {
        // 生成 java-callgraph2 使用的配置参数包装类
        JavaCG2ConfigureWrapper javaCG2ConfigureWrapper = TestConfigGenerator.genJavaCG2ConfigureWrapper();
        // java-callgraph2表达式开启调试
        javaCG2ConfigureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_EL_DEBUG_MODE, Boolean.TRUE.toString());
        for (JavaCG2ElConfigEnum javaCG2ElConfigEnum : JavaCG2ElConfigEnum.values()) {
            javaCG2ConfigureWrapper.setElConfigFixedTrue(javaCG2ElConfigEnum);
        }

        ConfigureWrapper configureWrapper = new ConfigureWrapper();
        // 设置基本的配置参数
        TestConfigGenerator.setBaseConfig(configureWrapper);
        // 使用H2数据库
        TestConfigGenerator.useH2Db(configureWrapper);

        // 尝试使用本地的配置参数
        JACGTestUtil.useLocalConfig(configureWrapper);

        Assert.assertTrue(new RunnerWriteDb(javaCG2ConfigureWrapper, configureWrapper).run());
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

        commonWriteDbForce();
    }

    @JACGExample(title = "仅解析指定包下的类的方法调用",
            desc = {"通过表达式实现，仅当类的包名以 test.callgraph.methodcall. 开头时不跳过"})
    @Test
    public void testElOnlyParseSomeMethodCall() {
        // java-callgraph2表达式开启调试
        javaCG2ConfigureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_EL_DEBUG_MODE, Boolean.TRUE.toString());
        javaCG2ConfigureWrapper.setElConfigText(JavaCG2ElConfigEnum.ECE_PARSE_IGNORE_METHOD_CALL,
                "!string.startsWith(" + CommonElAllowedVariableEnum.EAVE_MC_ER_PACKAGE_NAME.getVariableName() + ", 'test.callgraph.methodcall.')"
        );

        commonWriteDbForce();
    }

    @JACGExample(title = "仅解析目录中指定路径下指定名称的jar文件",
            desc = {"通过表达式实现，仅处理lib中文件名以commons-开头的jar文件",
                    "需要先执行以下命令生成包含jar文件的jar文件",
                    "gradlew gen_run_jar"})
    @Test
    public void testElOnlyParseSomeJarInDir() {
        javaCG2ConfigureWrapper.setOtherConfigList(JavaCG2OtherConfigFileUseListEnum.OCFULE_JAR_DIR, "jar_output_dir");
        // java-callgraph2表达式开启调试
        javaCG2ConfigureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_EL_DEBUG_MODE, Boolean.TRUE.toString());
        javaCG2ConfigureWrapper.setElConfigText(JavaCG2ElConfigEnum.ECE_MERGE_FILE_IGNORE_JAR_IN_DIR,
                "!string.endsWith(" + JavaCG2ElAllowedVariableEnum.EAVE_MF_ABSOLUTE_FILE_DIR_PATH_IN_DIR.getVariableName() + ", '/lib')" +
                        " || !string.startsWith(" + JavaCG2ElAllowedVariableEnum.EAVE_MF_FILE_NAME.getVariableName() + ", 'commons-')"
        );

        commonWriteDbForce();
    }

    @JACGExample(title = "仅解析jar文件中指定路径下的jar文件",
            desc = {"通过表达式实现，当jar文件的目录名称为'lib'时跳过",
                    "需要先执行以下命令生成包含jar文件的jar文件",
                    "gradlew gen_run_jar gen_jar_in_jar"})
    @Test
    public void testElOnlyParseNonLibJarInJar() {
        javaCG2ConfigureWrapper.setOtherConfigList(JavaCG2OtherConfigFileUseListEnum.OCFULE_JAR_DIR, "build/jar_output.jar");
        // java-callgraph2表达式开启调试
        javaCG2ConfigureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_EL_DEBUG_MODE, Boolean.TRUE.toString());
        javaCG2ConfigureWrapper.setElConfigText(JavaCG2ElConfigEnum.ECE_MERGE_FILE_IGNORE_JAR_IN_JAR_WAR,
                JavaCG2ElAllowedVariableEnum.EAVE_MF_FILE_DIR_PATH_IN_JAR_WAR.getVariableName() + " == 'lib'"
        );

        commonWriteDbForce();
    }

    @JACGExample(title = "仅解析jar文件中指定路径下的jar文件",
            desc = {"通过表达式实现，当jar文件的目录名称不是'lib'，或当jar文件名称以'commons-'开头时才解析",
                    "需要先执行以下命令生成包含jar文件的jar文件",
                    "gradlew gen_run_jar gen_jar_in_jar"})
    @Test
    public void testElOnlyParseLibJarInJar() {
        javaCG2ConfigureWrapper.setOtherConfigList(JavaCG2OtherConfigFileUseListEnum.OCFULE_JAR_DIR, "build/jar_output.jar");
        // java-callgraph2表达式开启调试
        javaCG2ConfigureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_EL_DEBUG_MODE, Boolean.TRUE.toString());
        javaCG2ConfigureWrapper.setElConfigText(JavaCG2ElConfigEnum.ECE_MERGE_FILE_IGNORE_JAR_IN_JAR_WAR,
                JavaCG2ElAllowedVariableEnum.EAVE_MF_FILE_DIR_PATH_IN_JAR_WAR.getVariableName() + " == 'lib'" +
                        " && !string.startsWith(" + JavaCG2ElAllowedVariableEnum.EAVE_MF_FILE_NAME.getVariableName() + ", 'commons-')"
        );

        commonWriteDbForce();
    }

    @JACGExample(title = "仅解析war文件中指定路径下的jar文件",
            desc = {"通过表达式实现，当jar文件的目录名称为'WEB-INF/lib'时跳过",
                    "需要先执行以下命令生成包含jar文件的war文件",
                    "gradlew gen_run_jar gen_jar_in_war"})
    @Test
    public void testElOnlyParseNonLibJarInWar() {
        javaCG2ConfigureWrapper.setOtherConfigList(JavaCG2OtherConfigFileUseListEnum.OCFULE_JAR_DIR, "build/jar_output.war");
        // java-callgraph2表达式开启调试
        javaCG2ConfigureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_EL_DEBUG_MODE, Boolean.TRUE.toString());
        javaCG2ConfigureWrapper.setElConfigText(JavaCG2ElConfigEnum.ECE_MERGE_FILE_IGNORE_JAR_IN_JAR_WAR,
                JavaCG2ElAllowedVariableEnum.EAVE_MF_FILE_DIR_PATH_IN_JAR_WAR.getVariableName() + " == 'WEB-INF/lib'"
        );

        commonWriteDbForce();
    }
}
