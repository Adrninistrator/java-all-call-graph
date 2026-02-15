package test.runbycodemain;

import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.runner.RunnerWriteDb;
import com.adrninistrator.javacg2.conf.JavaCG2ConfigureWrapper;
import com.adrninistrator.javacg2.conf.enums.JavaCG2OtherConfigFileUseListEnum;
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
@JACGExample(title = "解析代码并将结果写入数据库",
        desc = {"需要先执行 unittest.gradle 中的命令，生成示例jar包： gradlew test_gen_jar",
                "通过代码指定配置参数的主要功能示例"})
public class TestRBC1RunnerWriteDb extends TestRunByCodeBase {

    // 使用最简单的参数
    @Test
    public void $test0WriteDb() {
        // 生成 java-callgraph2 使用的配置参数包装类
        JavaCG2ConfigureWrapper javaCG2ConfigureWrapper = new JavaCG2ConfigureWrapper();
        javaCG2ConfigureWrapper.setOtherConfigList(JavaCG2OtherConfigFileUseListEnum.OCFULE_JAR_DIR,
                TestConfigGenerator.TEST_JAR_PATH);

        ConfigureWrapper configureWrapper = new ConfigureWrapper();
        Assert.assertTrue(new RunnerWriteDb(javaCG2ConfigureWrapper, configureWrapper).run());
    }

    // 支持使用本地参数
    @Test
    public void test2WriteDbLocal() {
        // 生成 java-callgraph2 使用的配置参数包装类
        JavaCG2ConfigureWrapper javaCG2ConfigureWrapper = TestConfigGenerator.genJavaCG2ConfigureWrapper();

        ConfigureWrapper configureWrapper = new ConfigureWrapper();
        // 设置基本的配置参数
        TestConfigGenerator.setBaseConfig(configureWrapper);
        // 使用H2数据库
        TestConfigGenerator.useH2Db(configureWrapper);

        // 尝试使用本地的配置参数
        JACGTestUtil.useLocalConfig(configureWrapper);

        Assert.assertTrue(new RunnerWriteDb(javaCG2ConfigureWrapper, configureWrapper).run());
    }
}
