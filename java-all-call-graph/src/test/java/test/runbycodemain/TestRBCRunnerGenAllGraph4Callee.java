package test.runbycodemain;

import com.adrninistrator.jacg.conf.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.conf.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.jacg.dto.methodcall.MethodCallLineData4Ee;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Callee;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.junit.Assert;
import org.junit.Test;
import test.annotation.JACGExample;
import test.runbycode.base.TestRunByCodeBase;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2025/2/16
 * @description:
 */
@JACGExample(title = "生成指定方法向上的完整方法调用链",
        desc = {"通过代码指定配置参数的主要功能示例"})
public class TestRBCRunnerGenAllGraph4Callee extends TestRunByCodeBase {

    @JACGExample(title = "方法调用链数据仅写入文件",
            desc = {"方法调用链数据不在内存中返回"})
    @Test
    public void testWriteToFile() {
        int callGraphDirNumBefore = getCallGraphDirNum4Ee();
        Assert.assertTrue(new RunnerGenAllGraph4Callee(configureWrapper).run());
        int callGraphDirNumAfter = getCallGraphDirNum4Ee();
        Assert.assertEquals(callGraphDirNumAfter, callGraphDirNumBefore + 1);
    }

    @JACGExample(title = "方法调用链数据仅写入文件，生成文件名使用更短的模式",
            desc = {"方法调用链数据不在内存中返回"})
    @Test
    public void testWriteToFileShortName() {
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_FILE_SHORT_MODE, Boolean.TRUE.toString());
        Assert.assertTrue(new RunnerGenAllGraph4Callee(configureWrapper).run());
    }

    @JACGExample(title = "方法调用链数据仅在内存中返回",
            desc = {"方法调用链数据不写入文件"})
    @Test
    public void testReturnInMemory() {
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_WRITE_TO_FILE, Boolean.FALSE.toString());
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_RETURN_IN_MEMORY, Boolean.TRUE.toString());

        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLEE,
                System.class.getName() + ":getProperty(java.lang.String)");

        RunnerGenAllGraph4Callee runnerGenAllGraph4Callee = new RunnerGenAllGraph4Callee(configureWrapper);
        int callGraphDirNumBefore = getCallGraphDirNum4Ee();
        Assert.assertTrue(runnerGenAllGraph4Callee.run());
        int callGraphDirNumAfter = getCallGraphDirNum4Ee();
        Assert.assertEquals(callGraphDirNumAfter, callGraphDirNumBefore);
        List<MethodCallLineData4Ee> allMethodCallLineData4EeList = runnerGenAllGraph4Callee.getAllMethodCallLineData4EeList();
        printListContent(allMethodCallLineData4EeList);
        Assert.assertFalse(JavaCG2Util.isCollectionEmpty(allMethodCallLineData4EeList));
    }

    @JACGExample(title = "方法调用链数据写入文件，也在内存中返回",
            desc = {})
    @Test
    public void testBoth() {
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_WRITE_TO_FILE, Boolean.TRUE.toString());
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_RETURN_IN_MEMORY, Boolean.TRUE.toString());

        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLEE,
                System.class.getName() + ":getProperty(java.lang.String)");

        RunnerGenAllGraph4Callee runnerGenAllGraph4Callee = new RunnerGenAllGraph4Callee(configureWrapper);
        int callGraphDirNumBefore = getCallGraphDirNum4Ee();
        Assert.assertTrue(runnerGenAllGraph4Callee.run());
        int callGraphDirNumAfter = getCallGraphDirNum4Ee();
        Assert.assertEquals(callGraphDirNumAfter, callGraphDirNumBefore + 1);
        List<MethodCallLineData4Ee> allMethodCallLineData4EeList = runnerGenAllGraph4Callee.getAllMethodCallLineData4EeList();
        printListContent(allMethodCallLineData4EeList);
        Assert.assertFalse(JavaCG2Util.isCollectionEmpty(allMethodCallLineData4EeList));
    }
}
