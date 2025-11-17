package test.runbycodemain;

import com.adrninistrator.jacg.conf.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.conf.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.jacg.dto.methodcall.MethodCallLineData4Ee;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Callee;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.junit.Assert;
import org.junit.Test;
import test.annotation.JACGExample;
import test.callgraph.empty.TestEmptyClass1;
import test.runbycode.base.TestRunByCodeBase;

import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2025/2/16
 * @description:
 */
@JACGExample(title = "生成指定方法向上的方法完整调用链",
        desc = {"指定的方法或类不存在",
                "通过代码指定配置参数的主要功能示例"})
public class TestRBCRunnerGenAllGraph4CalleeNotFound extends TestRunByCodeBase {

    private static final String[] TASKS = new String[]{
            TestEmptyClass1.class.getName() + ":test133333()",
            "a:test133333()",
            "a:123",
            "a"
    };

    @Test
    public void testOnlyWriteFile() {
        testGenCallGraph(true, false);
    }

    @Test
    public void testOnlyReturnInMemory() {
        testGenCallGraph(false, true);
    }

    @Test
    public void testBoth() {
        testGenCallGraph(true, true);
    }

    private void testGenCallGraph(boolean writeFile, boolean returnInMemory) {
        for (String task : TASKS) {
            configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_WRITE_TO_FILE, (writeFile ? Boolean.TRUE.toString() : Boolean.FALSE.toString()));
            configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_RETURN_IN_MEMORY, (returnInMemory ? Boolean.TRUE.toString() : Boolean.FALSE.toString()));

            configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLEE, task);
            RunnerGenAllGraph4Callee runnerGenAllGraph4Callee = new RunnerGenAllGraph4Callee(configureWrapper);
            Assert.assertTrue(runnerGenAllGraph4Callee.run());
            if (returnInMemory) {
                Map<String, List<MethodCallLineData4Ee>> allMethodCallLineData4EeMap = runnerGenAllGraph4Callee.getAllMethodCallLineData4EeMap();
                for (Map.Entry<String, List<MethodCallLineData4Ee>> entry : allMethodCallLineData4EeMap.entrySet()) {
                    List<MethodCallLineData4Ee> methodCallLineData4EeList = entry.getValue();
                    Assert.assertTrue(JavaCG2Util.isCollectionEmpty(methodCallLineData4EeList));
                }
            }
        }
    }
}
