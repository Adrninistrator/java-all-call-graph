package test.runbycodemain;

import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.conf.enums.OtherConfigFileUseListEnum;
import com.adrninistrator.jacg.conf.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.jacg.dto.callstack.CallStackFileResult;
import com.adrninistrator.jacg.findstack.FindCallStackUpAndDown;
import org.junit.Assert;
import org.junit.Test;
import test.runbycode.base.TestRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2025/12/7
 * @description:
 */
public class TestRBCFindCallStackUpAndDown extends TestRunByCodeBase {

    @Test
    public void testExample() {
        ConfigureWrapper configureWrapper = new ConfigureWrapper();
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLEE,
                System.class.getName() + ":exit("
        );
        configureWrapper.setOtherConfigList(OtherConfigFileUseListEnum.OCFULE_FIND_STACK_KEYWORD_4ER,
                System.class.getName() + ":getProperty("
        );
        FindCallStackUpAndDown findCallStackUpAndDown = new FindCallStackUpAndDown(configureWrapper);
        CallStackFileResult callStackFileResult = findCallStackUpAndDown.find();
        Assert.assertTrue(callStackFileResult.isSuccess());
    }

    @Test
    public void test() {
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLEE,
                System.class.getName() + ":exit("
        );
        configureWrapper.setOtherConfigList(OtherConfigFileUseListEnum.OCFULE_FIND_STACK_KEYWORD_4ER,
                System.class.getName() + ":getProperty("
        );
        FindCallStackUpAndDown findCallStackUpAndDown = new FindCallStackUpAndDown(configureWrapper);
        CallStackFileResult callStackFileResult = findCallStackUpAndDown.find();
        Assert.assertTrue(callStackFileResult.isSuccess());
    }
}
