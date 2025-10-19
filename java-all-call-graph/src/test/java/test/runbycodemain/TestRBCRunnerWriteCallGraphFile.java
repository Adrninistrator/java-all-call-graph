package test.runbycodemain;

import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.runner.RunnerWriteCallGraphFile;
import com.adrninistrator.javacg2.conf.JavaCG2ConfigureWrapper;
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

    @Test
    public void test() {
        // 生成使用 java-callgraph2 的配置参数包装类
        JavaCG2ConfigureWrapper javaCG2ConfigureWrapper = TestConfigGenerator.genJavaCG2ConfigureWrapper();

        ConfigureWrapper configureWrapper = new ConfigureWrapper();
        // 设置基本的配置参数
        TestConfigGenerator.setBaseConfig(configureWrapper);

        Assert.assertTrue(new RunnerWriteCallGraphFile(javaCG2ConfigureWrapper, configureWrapper).run());
    }
}
