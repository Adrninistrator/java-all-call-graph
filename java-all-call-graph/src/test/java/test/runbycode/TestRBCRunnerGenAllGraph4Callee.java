package test.runbycode;

import com.adrninistrator.jacg.common.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Callee;
import org.junit.Assert;
import org.junit.Test;
import test.annotation.JACGExample;
import test.runbycode.base.TestRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2022/4/20
 * @description:
 */
@JACGExample(title = "生成指定方法向上的完整方法调用链",
        desc = {})
public class TestRBCRunnerGenAllGraph4Callee extends TestRunByCodeBase {
    @Test
    public void test() {
        Assert.assertTrue(new RunnerGenAllGraph4Callee(configureWrapper).run());
    }

    @Test
    public void testAll() {
        ConfigureWrapper configureWrapperCopy = configureWrapper.copy();
        configureWrapperCopy.setAllowAllClasses();
        Assert.assertTrue(new RunnerGenAllGraph4Callee(configureWrapperCopy).run());
    }

    @Test
    public void testAllLimit() {
        ConfigureWrapper configureWrapperCopy = configureWrapper.copy();
        configureWrapperCopy.setAllowAllClasses();
        configureWrapperCopy.setMainConfig(ConfigKeyEnum.GEN_CALL_GRAPH_NUM_LIMIT, "10");
        Assert.assertTrue(new RunnerGenAllGraph4Callee(configureWrapperCopy).run());
    }
}
