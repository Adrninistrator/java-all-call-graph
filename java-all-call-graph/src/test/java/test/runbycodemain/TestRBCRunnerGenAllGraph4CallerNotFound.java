package test.runbycodemain;

import com.adrninistrator.jacg.conf.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.conf.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.jacg.dto.methodcall.MethodCallLineData4Er;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Caller;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.junit.Assert;
import org.junit.Test;
import test.annotation.JACGExample;
import test.callgraph.empty.TestEmptyClass1;
import test.runbycode.base.TestRunByCodeBase;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2025/2/16
 * @description:
 */
@JACGExample(title = "生成指定方法向下的完整方法调用链",
        desc = {"指定的方法或类不存在",
                "通过代码指定配置参数的主要功能示例"})
public class TestRBCRunnerGenAllGraph4CallerNotFound extends TestRunByCodeBase {

    private static final String[] TASKS = new String[]{
            TestEmptyClass1.class.getName() + ":test133333()",
            "a:test133333()",
            "a:test133333() 139-492",
            "a:123",
            "a:123 139-492",
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

            configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLER, task);
            RunnerGenAllGraph4Caller runnerGenAllGraph4Caller = new RunnerGenAllGraph4Caller(configureWrapper);
            Assert.assertTrue(runnerGenAllGraph4Caller.run());
            if (returnInMemory) {
                List<MethodCallLineData4Er> allMethodCallLineData4ErList = runnerGenAllGraph4Caller.getAllMethodCallLineData4ErList();
                Assert.assertTrue(JavaCG2Util.isCollectionEmpty(allMethodCallLineData4ErList));
            }
        }
    }
}
