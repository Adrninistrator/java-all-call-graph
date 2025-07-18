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
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2025/5/12
 * @description:
 */
@JACGExample(title = "生成指定方法向下的完整方法调用链",
        desc = {"限制允许生成的方法调用链深度限制",
                "通过代码指定配置参数的主要功能示例"})
public class TestRBCRunnerGenAllGraph4CallerLimitDepth extends TestRunByCodeBase {

    @JACGExample(title = "方法调用链数据仅写入文件",
            desc = {"方法调用链数据不在内存中返回"})
    @Test
    public void testWriteToFile() {
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_GEN_CALL_GRAPH_DEPTH_LIMIT, "2");
        Assert.assertTrue(new RunnerGenAllGraph4Caller(configureWrapper).run());
    }

    @JACGExample(title = "方法调用链数据仅在内存中返回",
            desc = {"方法调用链数据不写入文件"})
    @Test
    public void testReturnInMemory() {
        int limitNum = 2;
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_GEN_CALL_GRAPH_DEPTH_LIMIT, String.valueOf(limitNum));

        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_WRITE_TO_FILE, Boolean.FALSE.toString());
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_RETURN_IN_MEMORY, Boolean.TRUE.toString());

        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLER,
                TestExtendComplex.class.getName() + ":test1()");

        RunnerGenAllGraph4Caller runnerGenAllGraph4Caller = new RunnerGenAllGraph4Caller(configureWrapper);
        Assert.assertTrue(runnerGenAllGraph4Caller.run());
        Map<String, List<MethodCallLineData4Er>> allMethodCallLineData4ErMap = runnerGenAllGraph4Caller.getAllMethodCallLineData4ErMap();
        printMapContent(allMethodCallLineData4ErMap);
    }
}
