package test.runbycodemain;

import com.adrninistrator.jacg.common.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.common.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.findstack.FindCallStackTrace;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Callee;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Caller;
import com.adrninistrator.jacg.runner.RunnerWriteCallGraphFile;
import com.adrninistrator.jacg.runner.RunnerWriteDb;
import com.adrninistrator.javacg2.common.enums.JavaCG2ConfigKeyEnum;
import com.adrninistrator.javacg2.conf.JavaCG2ConfigureWrapper;
import org.junit.Assert;
import test.callgraph.empty.TestEmptyClass1;
import test.runbycode.base.TestRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2025/1/19
 * @description:
 */
public abstract class TestRBCBase extends TestRunByCodeBase {

    public void $test0RunnerWriteDb() {
        Assert.assertTrue(new RunnerWriteDb(configureWrapper).run());
    }

    public void testRunnerSimpleWriteDb() {
        JavaCG2ConfigureWrapper javaCG2ConfigureWrapper = configureWrapper.genJavaCG2ConfigureWrapper();
        javaCG2ConfigureWrapper.setConfig(JavaCG2ConfigKeyEnum.CKE_PARSE_METHOD_CALL_TYPE_VALUE, Boolean.FALSE.toString());

        ConfigureWrapper configureWrapperCopy = configureWrapper.copy();
        configureWrapperCopy.setMainConfig(ConfigKeyEnum.CKE_PARSE_OTHER_TYPE_FILE, Boolean.FALSE.toString());

        Assert.assertTrue(new RunnerWriteDb(configureWrapperCopy).run(javaCG2ConfigureWrapper));
    }

    public void testRunnerWriteFile() {
        JavaCG2ConfigureWrapper javaCG2ConfigureWrapper = configureWrapper.genJavaCG2ConfigureWrapper();
        javaCG2ConfigureWrapper.setConfig(JavaCG2ConfigKeyEnum.CKE_PARSE_METHOD_CALL_TYPE_VALUE, Boolean.TRUE.toString());

        ConfigureWrapper configureWrapperCopy = configureWrapper.copy();
        configureWrapperCopy.setMainConfig(ConfigKeyEnum.CKE_PARSE_OTHER_TYPE_FILE, Boolean.TRUE.toString());

        Assert.assertTrue(new RunnerWriteCallGraphFile(configureWrapperCopy).run(javaCG2ConfigureWrapper));
    }

    public void testFindCallStackTrace4ee() {
        runFindCallStackTraceAndCheck(new FindCallStackTrace(true, configureWrapper));
    }

    public void testFindCallStackTrace4er() {
        runFindCallStackTraceAndCheck(new FindCallStackTrace(false, configureWrapper));
    }

    public void testRunnerGenAllGraph4Callee() {
        Assert.assertTrue(new RunnerGenAllGraph4Callee(configureWrapper).run());
    }

    public void testRunnerGenAllGraph4CalleeLimit() {
        ConfigureWrapper configureWrapperCopy = configureWrapper.copy();
        configureWrapperCopy.setMainConfig(ConfigKeyEnum.CKE_GEN_CALL_GRAPH_NUM_LIMIT, "10");
        Assert.assertTrue(new RunnerGenAllGraph4Callee(configureWrapperCopy).run());
    }

    public void testRunnerGenAllGraph4CalleeEmptyClass() {
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLEE,
                TestEmptyClass1.class.getName() + ":test133333()"
        );

        Assert.assertTrue(new RunnerGenAllGraph4Callee(configureWrapper).run());
    }

    public void testRunnerGenAllGraph4Caller() {
        Assert.assertTrue(new RunnerGenAllGraph4Caller(configureWrapper).run());
    }

    public void testRunnerGenAllGraph4CallerLimit() {
        ConfigureWrapper configureWrapperCopy = configureWrapper.copy();
        configureWrapperCopy.setMainConfig(ConfigKeyEnum.CKE_GEN_CALL_GRAPH_NUM_LIMIT, "10");
        Assert.assertTrue(new RunnerGenAllGraph4Caller(configureWrapperCopy).run());
    }

    public void testRunnerGenAllGraph4CallerEmptyClass() {
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLER,
                TestEmptyClass1.class.getName() + ":test133333()"
        );

        Assert.assertTrue(new RunnerGenAllGraph4Caller(configureWrapper).run());
    }

    public void testRunnerWriteCallGraphFile() {
        Assert.assertTrue(new RunnerWriteCallGraphFile(configureWrapper).run());
    }
}
