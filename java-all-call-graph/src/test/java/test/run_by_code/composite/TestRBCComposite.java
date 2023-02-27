package test.run_by_code.composite;

import com.adrninistrator.jacg.common.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.find_keyword.FindKeywordCallGraph;
import com.adrninistrator.jacg.runner.RunnerWriteDb;
import com.adrninistrator.jacg.util.JACGUtil;
import org.junit.Assert;
import org.junit.FixMethodOrder;
import org.junit.Test;
import org.junit.runners.MethodSorters;
import test.run_by_code.TestRunByCodeBase;

import java.util.Collections;
import java.util.HashSet;

/**
 * @author adrninistrator
 * @date 2022/11/13
 * @description:
 */
@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public class TestRBCComposite extends TestRunByCodeBase {

    @Test
    public void test0WriteDb() {
        Assert.assertTrue(new RunnerWriteDb().run(configureWrapper, javaCGConfigureWrapper));
    }

    @Test
    public void test1NoIgnore() {
//        configureWrapper.setConfig(ConfigKeyEnum.CKE_OUTPUT_ROOT_PATH,"D:/desktop");
        ConfigureWrapper configureWrapperCopy = configureWrapper.copy();

        new FindKeywordCallGraph().find(true, configureWrapperCopy);
        new FindKeywordCallGraph().find(false, configureWrapperCopy);
    }

    @Test
    public void test2WithIgnore() {
        ConfigureWrapper configureWrapperCopy = configureWrapper.copy();
        configureWrapperCopy.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_IGNORE_CLASS_KEYWORD, JACGUtil.genSetFromArray(
                "TestArgument1"));
        configureWrapperCopy.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_IGNORE_FULL_METHOD_PREFIX, JACGUtil.genSetFromArray(
                "test.call_graph.argument.TestArgument2"));
        configureWrapperCopy.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_IGNORE_METHOD_PREFIX, JACGUtil.genSetFromArray(
                "test1("));

        new FindKeywordCallGraph().find(true, configureWrapperCopy);
        new FindKeywordCallGraph().find(false, configureWrapperCopy);
    }
}
