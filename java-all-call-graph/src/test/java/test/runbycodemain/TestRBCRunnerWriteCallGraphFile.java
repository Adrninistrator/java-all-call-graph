package test.runbycodemain;

import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.runner.RunnerWriteCallGraphFile;
import com.adrninistrator.javacg2.conf.JavaCG2ConfigureWrapper;
import com.adrninistrator.javacg2.conf.enums.JavaCG2OtherConfigFileUseListEnum;
import org.junit.Assert;
import org.junit.Test;
import test.annotation.JACGExample;
import test.runbycode.base.TestRunByCodeBase;
import test.runbycode.config.TestConfigGenerator;

/**
 * @author adrninistrator
 * @date 2025/2/16
 * @description:
 */
@JACGExample(title = "解析代码并将结果写入文件",
        desc = {"解析类、方法、方法调用等信息",
                "生成的数据写入文件，但不写入数据库",
                "通过代码指定配置参数的主要功能示例"})
public class TestRBCRunnerWriteCallGraphFile extends TestRunByCodeBase {

    // 使用最简单的参数
    @Test
    public void $test0WriteFile() {
        // 生成 java-callgraph2 使用的配置参数包装类
        JavaCG2ConfigureWrapper javaCG2ConfigureWrapper = new JavaCG2ConfigureWrapper();
        javaCG2ConfigureWrapper.setOtherConfigList(JavaCG2OtherConfigFileUseListEnum.OCFULE_JAR_DIR,
                TestConfigGenerator.TEST_JAR_PATH);

        ConfigureWrapper configureWrapper = new ConfigureWrapper();
        Assert.assertTrue(new RunnerWriteCallGraphFile(javaCG2ConfigureWrapper, configureWrapper).run());
    }

    @Test
    public void test2WriteFile() {
        // 生成 java-callgraph2 使用的配置参数包装类
        JavaCG2ConfigureWrapper javaCG2ConfigureWrapper = TestConfigGenerator.genJavaCG2ConfigureWrapper();

        ConfigureWrapper configureWrapper = new ConfigureWrapper();
        // 设置基本的配置参数
        TestConfigGenerator.setBaseConfig(configureWrapper);

        Assert.assertTrue(new RunnerWriteCallGraphFile(javaCG2ConfigureWrapper, configureWrapper).run());
    }
}
