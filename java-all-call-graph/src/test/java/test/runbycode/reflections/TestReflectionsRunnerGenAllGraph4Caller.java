package test.runbycode.reflections;

import com.adrninistrator.jacg.conf.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Caller;
import org.junit.Assert;
import org.junit.Test;
import test.callgraph.reflections.TestReflectionCaller;
import test.callgraph.reflections.TestReflectionCallerWrapper;
import test.runbycode.base.TestRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2026/2/8
 * @description:
 */
public class TestReflectionsRunnerGenAllGraph4Caller extends TestRunByCodeBase {

    @Test
    public void testWriteToFile() {
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLER,
                TestReflectionCaller.class.getName(),
                TestReflectionCallerWrapper.class.getName()
        );

        Assert.assertTrue(new RunnerGenAllGraph4Caller(configureWrapper).run());
    }
}
