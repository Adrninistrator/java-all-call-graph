package test.runbycodemain;

import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.conf.enums.OtherConfigFileUseListEnum;
import com.adrninistrator.jacg.conf.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.jacg.dto.callstack.CallStackFileResult;
import com.adrninistrator.jacg.findstack.FindCallStackTrace;
import org.junit.Assert;
import org.junit.Test;
import test.annotation.JACGExample;
import test.callgraph.branch.TestBranch1;
import test.runbycode.base.TestRunByCodeBase;
import test.runbycode.config.TestConfigGenerator;
import test.runbycode.util.JACGTestUtil;

/**
 * @author adrninistrator
 * @date 2025/2/16
 * @description:
 */
@JACGExample(title = "获得方法向下到包含关键字的调用堆栈",
        desc = {"首先会生成指定方法向下的方法完整调用链",
                "通过代码指定配置参数的主要功能示例"})
public class TestRBCFindCallStackTrace4er extends TestRunByCodeBase {

    @Test
    public void testExample() {
        ConfigureWrapper configureWrapper = new ConfigureWrapper();
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLER, TestBranch1.class.getName());
        configureWrapper.setOtherConfigList(OtherConfigFileUseListEnum.OCFULE_FIND_STACK_KEYWORD_4ER, System.class.getName()
        );

        FindCallStackTrace findCallStackTrace = new FindCallStackTrace(false, configureWrapper);
        CallStackFileResult callStackFileResult = findCallStackTrace.find();
        Assert.assertTrue(callStackFileResult.isSuccess());
    }

    @Test
    public void test() {
        ConfigureWrapper configureWrapper = new ConfigureWrapper();
        // 设置基本的配置参数
        TestConfigGenerator.setBaseConfig(configureWrapper);
        // 使用H2数据库
        TestConfigGenerator.useH2Db(configureWrapper);
        // 设置需要生成向下的方法完整调用链的类或方法
        TestConfigGenerator.setGenCallerGraphMethodClass(configureWrapper);
        // 设置生成向下的方法完整调用链文件后，从最顶层调用方法开始向下查找包含指定关键字的方法的调用堆栈时，使用的关键字
        TestConfigGenerator.setFindStackKeyword4er(configureWrapper);

        // 尝试使用本地的配置参数
        JACGTestUtil.useLocalConfig(configureWrapper);

        runFindCallStackTraceAndCheck(new FindCallStackTrace(false, configureWrapper));
    }
}
