package test.runbycode.ellimit;

import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.conf.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.conf.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.jacg.el.enums.ElConfigEnum;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Callee;
import org.junit.Assert;
import org.junit.Test;
import test.runbycode.base.TestRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2026/4/27
 * @description:
 */
public class TestElLimit1 extends TestRunByCodeBase {

    @Test
    public void test() {
        ConfigureWrapper configureWrapper = new ConfigureWrapper();
        configureWrapper.setElConfigFixedTrue(ElConfigEnum.ECE_GEN_ALL_CALL_GRAPH_IGNORE_METHOD_CALL);
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_EL_IGNORE_DATA_MAX_LINE_NUM, "20");
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLEE,
                System.class.getName()
        );

        RunnerGenAllGraph4Callee runnerGenAllGraph4Callee = new RunnerGenAllGraph4Callee(configureWrapper);
        Assert.assertTrue(runnerGenAllGraph4Callee.run());
    }
}
