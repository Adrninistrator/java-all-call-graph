package test.runbycodemain;

import com.adrninistrator.jacg.common.enums.OutputDetailEnum;
import com.adrninistrator.jacg.conf.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Callee;
import org.junit.Assert;
import org.junit.Test;
import test.annotation.JACGExample;
import test.runbycode.base.TestRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2025/4/6
 * @description:
 */
@JACGExample(title = "生成指定方法向上的完整方法调用链",
        desc = {"输出方法调用链格式使用最详细，包含返回类型",
                "通过代码指定配置参数的主要功能示例"})
public class TestRBCRunnerGenAllGraph4CalleeDetail0 extends TestRunByCodeBase {

    @Test
    public void test() {
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_OUTPUT_DETAIL, OutputDetailEnum.ODE_0.getDetail());
        Assert.assertTrue(new RunnerGenAllGraph4Callee(configureWrapper).run());
    }
}