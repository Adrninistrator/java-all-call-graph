package test.runbycodemain;

import com.adrninistrator.jacg.conf.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Caller;
import org.junit.Assert;
import org.junit.Test;
import test.annotation.JACGExample;
import test.callgraph.empty.TestEmptyClass1;
import test.callgraph.empty.TestNoMethodClass1;
import test.runbycode.base.TestRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2025/3/8
 * @description:
 */
@JACGExample(title = "生成指定方法向下的方法完整调用链",
        desc = {"指定的方法未调用其他方法",
                "通过代码指定配置参数的主要功能示例"})
public class TestRBCRunnerGenAllGraph4CallerEmpty extends TestRunByCodeBase {

    @Test
    public void test() {
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLER,
                TestEmptyClass1.class.getName() + ":testEmpty()",
                TestNoMethodClass1.class.getName()
        );
        Assert.assertTrue(new RunnerGenAllGraph4Caller(configureWrapper).run());
    }
}
