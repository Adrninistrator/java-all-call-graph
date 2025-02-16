package test.runbycodemain;

import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Caller;
import org.junit.Assert;
import org.junit.Test;
import test.annotation.JACGExample;
import test.runbycode.base.TestRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2025/2/16
 * @description:
 */
@JACGExample(title = "生成指定方法向下的完整方法调用链",
        desc = {})
public class TestRunnerGenAllGraph4Caller extends TestRunByCodeBase {

    @Test
    public void test() {
        Assert.assertTrue(new RunnerGenAllGraph4Caller(configureWrapper).run());
    }
}
