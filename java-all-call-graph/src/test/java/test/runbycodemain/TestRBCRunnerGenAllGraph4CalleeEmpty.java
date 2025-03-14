package test.runbycodemain;

import com.adrninistrator.jacg.conf.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Callee;
import org.junit.Assert;
import org.junit.Test;
import test.annotation.JACGExample;
import test.callgraph.diffjar.controller.TestController1;
import test.callgraph.empty.TestNoMethodClass1;
import test.runbycode.base.TestRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2025/3/8
 * @description:
 */
@JACGExample(title = "生成指定方法向上的完整方法调用链",
        desc = {"指定的方法未被其他方法调用",
                "通过代码指定配置参数的主要功能示例"})
public class TestRBCRunnerGenAllGraph4CalleeEmpty extends TestRunByCodeBase {

    @Test
    public void test() {
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLEE,
                TestController1.class.getName() + ":get1()",
                TestNoMethodClass1.class.getName()
        );
        Assert.assertTrue(new RunnerGenAllGraph4Callee(configureWrapper).run());
    }
}
