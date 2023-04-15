package test.run_by_code.composite;

import com.adrninistrator.jacg.common.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.find_stack.FindCallStackTrace;
import com.adrninistrator.jacg.runner.RunnerWriteDb;
import org.junit.Assert;
import org.junit.FixMethodOrder;
import org.junit.Test;
import org.junit.runners.MethodSorters;
import test.call_graph.argument.TestArgument1;
import test.call_graph.argument.TestArgument2;
import test.run_by_code.base.TestRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2022/11/13
 * @description:
 */
@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public class TestRBCComposite extends TestRunByCodeBase {

    @Test
    public void test0WriteDb() {
        Assert.assertTrue(new RunnerWriteDb().run(configureWrapper));
    }

    @Test
    public void test1NoIgnore() {
//        configureWrapper.setConfig(ConfigKeyEnum.CKE_OUTPUT_ROOT_PATH,"D:/desktop");
        ConfigureWrapper configureWrapperCopy = configureWrapper.copy();

        new FindCallStackTrace().find(true, configureWrapperCopy);
        new FindCallStackTrace().find(false, configureWrapperCopy);
    }

    @Test
    public void test2WithIgnore() {
        ConfigureWrapper configureWrapperCopy = configureWrapper.copy();
        configureWrapperCopy.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_IGNORE_CLASS_KEYWORD,
                TestArgument1.class.getSimpleName());
        configureWrapperCopy.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_IGNORE_FULL_METHOD_PREFIX,
                TestArgument2.class.getName());
        configureWrapperCopy.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_IGNORE_METHOD_PREFIX,
                "test1(");

        new FindCallStackTrace().find(true, configureWrapperCopy);
        new FindCallStackTrace().find(false, configureWrapperCopy);
    }
}
