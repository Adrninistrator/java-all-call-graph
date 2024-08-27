package test.runbycode;

import com.adrninistrator.jacg.runner.RunnerWriteCallGraphFile;
import org.junit.Assert;
import org.junit.Test;
import test.annotation.JACGExample;
import test.runbycode.base.TestRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2023/4/2
 * @description:
 */
@JACGExample(title = "解析代码并将结果写入文件",
        desc = {"生成的数据不写入数据库"})
public class TestRBCRunnerWriteCallGraphFile extends TestRunByCodeBase {

    @Test
    public void test() {
        Assert.assertTrue(new RunnerWriteCallGraphFile(configureWrapper).run());
    }
}
