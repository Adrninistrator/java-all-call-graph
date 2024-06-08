package test.runbycode;

import com.adrninistrator.jacg.common.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Caller;
import org.junit.Assert;
import org.junit.Test;
import test.callgraph.empty.TestEmptyClass1;
import test.runbycode.base.TestRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2022/4/20
 * @description:
 */
public class TestRBCRunnerGenAllGraph4Caller extends TestRunByCodeBase {
    @Test
    public void test() {
        Assert.assertTrue(new RunnerGenAllGraph4Caller(configureWrapper).run());
    }

    @Test
    public void testEmptyClass() {
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLER,
//                TestEmptyClass1.class.getName() + ":test1()",
//                "aaa:bbb"

                TestEmptyClass1.class.getName() + ":test133333()"
        );

        Assert.assertTrue(new RunnerGenAllGraph4Caller(configureWrapper).run());
    }
}
