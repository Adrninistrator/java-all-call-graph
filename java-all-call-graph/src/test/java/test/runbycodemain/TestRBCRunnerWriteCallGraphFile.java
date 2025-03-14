package test.runbycodemain;

import com.adrninistrator.jacg.runner.RunnerWriteCallGraphFile;
import org.junit.Assert;
import org.junit.Test;
import test.annotation.JACGExample;
import test.runbycode.base.TestRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2025/2/16
 * @description:
 */
@JACGExample(title = "解析代码并将结果写入文件",
        desc = {"生成的数据不写入数据库",
                "通过代码指定配置参数的主要功能示例"})
public class TestRBCRunnerWriteCallGraphFile extends TestRunByCodeBase {

    @Test
    public void test() {
        Assert.assertTrue(new RunnerWriteCallGraphFile(javaCG2ConfigureWrapper, configureWrapper).run());
    }
}
