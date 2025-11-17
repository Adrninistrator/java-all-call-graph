package test.runbycodemain;

import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.conf.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.conf.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.jacg.dto.methodcall.MethodCallLineData4Er;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Caller;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.junit.Assert;
import org.junit.Test;
import test.annotation.JACGExample;
import test.callgraph.extendcomplex.TestExtendComplex;
import test.callgraph.methodcall.TestMCCaller;
import test.runbycode.base.TestRunByCodeBase;
import test.runbycode.config.TestConfigGenerator;
import test.runbycode.util.JACGTestUtil;

import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2025/2/16
 * @description:
 */
@JACGExample(title = "生成指定方法向下的方法完整调用链",
        desc = {"通过代码指定配置参数的主要功能示例"})
public class TestRBCRunnerGenAllGraph4Caller extends TestRunByCodeBase {

    @JACGExample(title = "方法完整调用链数据仅写入文件，示例代码",
            desc = {"方法完整调用链数据不在内存中返回"})
    @Test
    public void testExampleWriteToFile() {
        // 解析指定的Java代码，并将结果写入数据库
        new TestRBC1RunnerWriteDb().$test0WriteDb();

        ConfigureWrapper configureWrapper = new ConfigureWrapper();
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLER, TestMCCaller.class.getName());

        Assert.assertTrue(new RunnerGenAllGraph4Caller(configureWrapper).run());
    }

    @JACGExample(title = "方法完整调用链数据仅写入文件",
            desc = {"方法完整调用链数据不在内存中返回"})
    @Test
    public void testWriteToFile() {
        int callGraphDirNumBefore = getCallGraphDirNum4Er();

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

        Assert.assertTrue(new RunnerGenAllGraph4Caller(configureWrapper).run());
        int callGraphDirNumAfter = getCallGraphDirNum4Er();
        Assert.assertEquals(callGraphDirNumAfter, callGraphDirNumBefore + 1);
    }

    @JACGExample(title = "方法完整调用链数据仅写入文件，生成文件名使用更短的模式",
            desc = {"方法完整调用链数据不在内存中返回"})
    @Test
    public void testWriteToFileShortName() {
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_FILE_SHORT_MODE, Boolean.TRUE.toString());
        Assert.assertTrue(new RunnerGenAllGraph4Caller(configureWrapper).run());
    }

    @JACGExample(title = "方法完整调用链数据仅在内存中返回",
            desc = {"方法完整调用链数据不写入文件"})
    @Test
    public void testReturnInMemory() {
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLER,
                TestExtendComplex.class.getName() + ":test1()");
        doTestReturnInMemory();
    }

    @JACGExample(title = "方法完整调用链数据写入文件，也在内存中返回",
            desc = {})
    @Test
    public void testBoth() {
        doTestBoth();
    }

    public Map<String, List<MethodCallLineData4Er>> doTestReturnInMemory() {
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_WRITE_TO_FILE, Boolean.FALSE.toString());
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_RETURN_IN_MEMORY, Boolean.TRUE.toString());

        RunnerGenAllGraph4Caller runnerGenAllGraph4Caller = new RunnerGenAllGraph4Caller(configureWrapper);
        int callGraphDirNumBefore = getCallGraphDirNum4Er();
        Assert.assertTrue(runnerGenAllGraph4Caller.run());
        int callGraphDirNumAfter = getCallGraphDirNum4Er();
        Assert.assertEquals(callGraphDirNumAfter, callGraphDirNumBefore + 1);

        Map<String, List<MethodCallLineData4Er>> allMethodCallLineData4ErMap = runnerGenAllGraph4Caller.getAllMethodCallLineData4ErMap();
        printMapContent(allMethodCallLineData4ErMap);
        Assert.assertFalse(JavaCG2Util.isMapEmpty(allMethodCallLineData4ErMap));
        return allMethodCallLineData4ErMap;
    }

    public Map<String, List<MethodCallLineData4Er>> doTestBoth() {
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_WRITE_TO_FILE, Boolean.TRUE.toString());
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_RETURN_IN_MEMORY, Boolean.TRUE.toString());

        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLER,
                TestExtendComplex.class.getName() + ":test1()");

        RunnerGenAllGraph4Caller runnerGenAllGraph4Caller = new RunnerGenAllGraph4Caller(configureWrapper);

        int callGraphDirNumBefore = getCallGraphDirNum4Er();
        Assert.assertTrue(runnerGenAllGraph4Caller.run());
        int callGraphDirNumAfter = getCallGraphDirNum4Er();
        Assert.assertEquals(callGraphDirNumAfter, callGraphDirNumBefore + 1);

        Map<String, List<MethodCallLineData4Er>> allMethodCallLineData4ErMap = runnerGenAllGraph4Caller.getAllMethodCallLineData4ErMap();
        printMapContent(allMethodCallLineData4ErMap);
        Assert.assertFalse(JavaCG2Util.isMapEmpty(allMethodCallLineData4ErMap));
        return allMethodCallLineData4ErMap;
    }

    @JACGExample(title = "方法完整调用链数据仅在内存中返回，返回多个方法",
            desc = {"方法完整调用链数据不写入文件"})
    @Test
    public void testReturnInMemoryMulti() {
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLER,
                TestExtendComplex.class.getName());
        Map<String, List<MethodCallLineData4Er>> methodCallLineData4ErMap = doTestReturnInMemory();
        printSetContent(methodCallLineData4ErMap.keySet());
    }
}
