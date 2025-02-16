package test.runbycodemain;

import com.adrninistrator.jacg.conf.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Caller;
import org.junit.Assert;
import org.junit.Test;
import test.annotation.JACGExample;
import test.callgraph.empty.TestEmptyClass1;
import test.runbycode.base.TestRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2025/2/16
 * @description:
 */
@JACGExample(title = "生成指定方法向下的完整方法调用链",
        desc = {"生成结果为空"})
public class TestRunnerGenAllGraph4CallerEmptyClass extends TestRunByCodeBase {

    @Test
    public void test() {
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLER,
                TestEmptyClass1.class.getName() + ":test133333()"
        );

        Assert.assertTrue(new RunnerGenAllGraph4Caller(configureWrapper).run());
    }
}
