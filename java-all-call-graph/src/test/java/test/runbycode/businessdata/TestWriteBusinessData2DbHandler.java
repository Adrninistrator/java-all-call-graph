package test.runbycode.businessdata;

import com.adrninistrator.jacg.common.enums.DefaultBusinessDataTypeEnum;
import com.adrninistrator.jacg.conf.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Caller;
import org.junit.Assert;
import org.junit.Test;
import test.callgraph.branch.TestExceptions;
import test.callgraph.methodargument.TestArgument2;
import test.runbycode.base.TestRunByCodeBase;
import test.runbycode.businessdata.handler.WriteSystemSetProperty2DbHandler;
import test.runbycode.businessdata.handler.WriteTestArgument3Test1ToDbHandler;
import test.runbycode.businessdata.handler.WriteTestArgument3Test2ToDbHandler;

/**
 * @author adrninistrator
 * @date 2023/4/7
 * @description:
 */
public class TestWriteBusinessData2DbHandler extends TestRunByCodeBase {

    @Test
    public void $test0WriteDb() {
        commonWriteDb();
    }

    @Test
    public void testSystemSetProperty() {
        try (WriteSystemSetProperty2DbHandler writeSystemSetProperty2DbHandler = new WriteSystemSetProperty2DbHandler(configureWrapper)) {
            Assert.assertTrue(writeSystemSetProperty2DbHandler.handleMethodCallByCallee());
        }
    }

    @Test
    public void testTestArgument3Test1() {
        try (WriteTestArgument3Test1ToDbHandler writeTestArgument3Test1ToDbHandler = new WriteTestArgument3Test1ToDbHandler(configureWrapper)) {
            Assert.assertTrue(writeTestArgument3Test1ToDbHandler.handleMethodCallByCallee());
        }
    }

    @Test
    public void testTestArgument3Test2() {
        try (WriteTestArgument3Test2ToDbHandler writeTestArgument3Test2ToDbHandler = new WriteTestArgument3Test2ToDbHandler(configureWrapper)) {
            Assert.assertTrue(writeTestArgument3Test2ToDbHandler.handleMethodCallByCallee());
        }
    }

    @Test
    public void testZLastGenCallerGraph() {
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFULE_BUSINESS_DATA_TYPE_SHOW_4ER,
                DefaultBusinessDataTypeEnum.BDTE_METHOD_RETURN_GENERICS_TYPE.getType()
        );
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLER,
                TestExceptions.class.getName(),
                TestArgument2.class.getName()
        );
        RunnerGenAllGraph4Caller runnerGenAllGraph4Caller = new RunnerGenAllGraph4Caller(configureWrapper);
        Assert.assertTrue(runnerGenAllGraph4Caller.run());
    }
}
