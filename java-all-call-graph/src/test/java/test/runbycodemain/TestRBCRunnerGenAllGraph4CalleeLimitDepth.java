package test.runbycodemain;

import com.adrninistrator.jacg.conf.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.conf.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.jacg.dto.methodcall.MethodCallLineData4Ee;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Callee;
import org.junit.Assert;
import org.junit.Test;
import test.annotation.JACGExample;
import test.runbycode.base.TestRunByCodeBase;

import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2025/5/12
 * @description:
 */
@JACGExample(title = "生成指定方法向上的方法完整调用链",
        desc = {"限制允许生成的方法完整调用链深度限制",
                "通过代码指定配置参数的主要功能示例"})
public class TestRBCRunnerGenAllGraph4CalleeLimitDepth extends TestRunByCodeBase {

    @JACGExample(title = "方法完整调用链数据仅写入文件",
            desc = {"方法完整调用链数据不在内存中返回"})
    @Test
    public void testWriteToFile() {
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_GEN_CALL_GRAPH_DEPTH_LIMIT, "2");
        Assert.assertTrue(new RunnerGenAllGraph4Callee(configureWrapper).run());
    }

    @JACGExample(title = "方法完整调用链数据仅在内存中返回",
            desc = {"方法完整调用链数据不写入文件"})
    @Test
    public void testReturnInMemory() {
        int limitNum = 2;
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_GEN_CALL_GRAPH_DEPTH_LIMIT, String.valueOf(limitNum));

        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_WRITE_TO_FILE, Boolean.FALSE.toString());
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_RETURN_IN_MEMORY, Boolean.TRUE.toString());

        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLEE,
                System.class.getName() + ":getProperty(java.lang.String)");

        RunnerGenAllGraph4Callee runnerGenAllGraph4Callee = new RunnerGenAllGraph4Callee(configureWrapper);
        Assert.assertTrue(runnerGenAllGraph4Callee.run());
        Map<String, List<MethodCallLineData4Ee>> allMethodCallLineData4EeMap = runnerGenAllGraph4Callee.getAllMethodCallLineData4EeMap();
        printMapContent(allMethodCallLineData4EeMap);
    }
}
