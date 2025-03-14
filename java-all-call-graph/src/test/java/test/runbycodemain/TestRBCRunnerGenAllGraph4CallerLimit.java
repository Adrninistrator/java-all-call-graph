package test.runbycodemain;

import com.adrninistrator.jacg.conf.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.conf.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.jacg.dto.methodcall.MethodCallLineData4Er;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Caller;
import org.junit.Assert;
import org.junit.Test;
import test.annotation.JACGExample;
import test.callgraph.extendcomplex.TestExtendComplex;
import test.runbycode.base.TestRunByCodeBase;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2025/2/16
 * @description:
 */
@JACGExample(title = "生成指定方法向下的完整方法调用链",
        desc = {"限制每个方法允许生成的方法调用数量限制",
                "通过代码指定配置参数的主要功能示例"})
public class TestRBCRunnerGenAllGraph4CallerLimit extends TestRunByCodeBase {

    @JACGExample(title = "方法调用链数据仅写入文件",
            desc = {"方法调用链数据不在内存中返回"})
    @Test
    public void testWriteToFile() {
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_GEN_CALL_GRAPH_NUM_LIMIT, "10");
        Assert.assertTrue(new RunnerGenAllGraph4Caller(configureWrapper).run());
    }

    @JACGExample(title = "方法调用链数据仅在内存中返回",
            desc = {"方法调用链数据不写入文件"})
    @Test
    public void testReturnInMemory() {
        int limitNum = 10;
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_GEN_CALL_GRAPH_NUM_LIMIT, String.valueOf(limitNum));

        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_WRITE_TO_FILE, Boolean.FALSE.toString());
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_RETURN_IN_MEMORY, Boolean.TRUE.toString());

        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLER,
                TestExtendComplex.class.getName() + ":test1()");

        RunnerGenAllGraph4Caller runnerGenAllGraph4Caller = new RunnerGenAllGraph4Caller(configureWrapper);
        Assert.assertTrue(runnerGenAllGraph4Caller.run());
        List<MethodCallLineData4Er> allMethodCallLineData4ErList = runnerGenAllGraph4Caller.getAllMethodCallLineData4ErList();
        printListContent(allMethodCallLineData4ErList);
        Assert.assertEquals(limitNum, allMethodCallLineData4ErList.size() - 1);
    }
}
