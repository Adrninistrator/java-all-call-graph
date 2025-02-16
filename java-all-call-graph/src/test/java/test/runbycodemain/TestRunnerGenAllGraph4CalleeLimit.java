package test.runbycodemain;

import com.adrninistrator.jacg.conf.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Callee;
import org.junit.Assert;
import org.junit.Test;
import test.annotation.JACGExample;
import test.runbycode.base.TestRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2025/2/16
 * @description:
 */
@JACGExample(title = "生成指定方法向上的完整方法调用链",
        desc = {"限制每个方法允许生成的方法调用数量限制"})
public class TestRunnerGenAllGraph4CalleeLimit extends TestRunByCodeBase {

    @Test
    public void test() {
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_GEN_CALL_GRAPH_NUM_LIMIT, "10");
        Assert.assertTrue(new RunnerGenAllGraph4Callee(configureWrapper).run());
    }
}
