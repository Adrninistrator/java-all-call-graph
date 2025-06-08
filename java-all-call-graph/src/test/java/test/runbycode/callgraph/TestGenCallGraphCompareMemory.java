package test.runbycode.callgraph;

import com.adrninistrator.jacg.conf.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.jacg.dto.methodcall.MethodCallLineData4Ee;
import com.adrninistrator.jacg.dto.methodcall.MethodCallLineData4Er;
import com.adrninistrator.jacg.util.JACGJsonUtil;
import org.apache.commons.lang3.StringUtils;
import org.junit.Assert;
import org.junit.Test;
import test.callgraph.extendcomplex.TestExtendComplex;
import test.runbycode.base.TestRunByCodeBase;
import test.runbycodemain.TestRBCRunnerGenAllGraph4Callee;
import test.runbycodemain.TestRBCRunnerGenAllGraph4Caller;

import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2025/3/26
 * @description:
 */
public class TestGenCallGraphCompareMemory extends TestRunByCodeBase {

    private final TestRBCRunnerGenAllGraph4Callee testRBCRunnerGenAllGraph4Callee = new TestRBCRunnerGenAllGraph4Callee();
    private final TestRBCRunnerGenAllGraph4Caller testRBCRunnerGenAllGraph4Caller = new TestRBCRunnerGenAllGraph4Caller();

    @Test
    public void testCompareEe() {
        testRBCRunnerGenAllGraph4Callee.setConfigureWrapper(configureWrapper);
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLEE,
                System.class.getName() + ":getProperty(java.lang.String)");
        Map<String, List<MethodCallLineData4Ee>> methodCallLineData4EeMapInMemory = testRBCRunnerGenAllGraph4Callee.doTestReturnInMemory();
        Map<String, List<MethodCallLineData4Ee>> methodCallLineData4EeMapBoth = testRBCRunnerGenAllGraph4Callee.doTestBoth();

        if (methodCallLineData4EeMapInMemory.size() != methodCallLineData4EeMapBoth.size()) {
            Assert.fail("map长度不同");
        }

        for (Map.Entry<String, List<MethodCallLineData4Ee>> entry : methodCallLineData4EeMapInMemory.entrySet()) {
            String method = entry.getKey();
            List<MethodCallLineData4Ee> methodCallLineData4EeListInMemory = entry.getValue();
            List<MethodCallLineData4Ee> methodCallLineData4EeListBoth = methodCallLineData4EeMapBoth.get(method);
            if (methodCallLineData4EeListBoth == null) {
                Assert.fail("未找到list");
            }

            if (methodCallLineData4EeListInMemory.size() != methodCallLineData4EeListBoth.size()) {
                Assert.fail("list长度不同");
            }
            for (int i = 0; i < methodCallLineData4EeListInMemory.size(); i++) {
                String data1 = JACGJsonUtil.getJsonStr(methodCallLineData4EeListInMemory.get(i));
                String data2 = JACGJsonUtil.getJsonStr(methodCallLineData4EeListBoth.get(i));
                if (!StringUtils.equals(data1, data2)) {
                    Assert.fail("元素内容不相同 " + i);
                }
            }
        }
    }

    @Test
    public void testCompareEr() {
        testRBCRunnerGenAllGraph4Caller.setConfigureWrapper(configureWrapper);
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLER,
                TestExtendComplex.class.getName() + ":test1()");
        Map<String, List<MethodCallLineData4Er>> methodCallLineData4ErMapInMemory = testRBCRunnerGenAllGraph4Caller.doTestReturnInMemory();
        Map<String, List<MethodCallLineData4Er>> methodCallLineData4ErMapBoth = testRBCRunnerGenAllGraph4Caller.doTestBoth();

        if (methodCallLineData4ErMapInMemory.size() != methodCallLineData4ErMapBoth.size()) {
            Assert.fail("map长度不同");
        }

        for (Map.Entry<String, List<MethodCallLineData4Er>> entry : methodCallLineData4ErMapInMemory.entrySet()) {
            String method = entry.getKey();
            List<MethodCallLineData4Er> methodCallLineData4ErListInMemory = entry.getValue();
            List<MethodCallLineData4Er> methodCallLineData4ErListBoth = methodCallLineData4ErMapBoth.get(method);
            if (methodCallLineData4ErListBoth == null) {
                Assert.fail("未找到list");
            }

            if (methodCallLineData4ErListInMemory.size() != methodCallLineData4ErListBoth.size()) {
                Assert.fail("list长度不同");
            }
            for (int i = 0; i < methodCallLineData4ErListInMemory.size(); i++) {
                String data1 = JACGJsonUtil.getJsonStr(methodCallLineData4ErListInMemory.get(i));
                String data2 = JACGJsonUtil.getJsonStr(methodCallLineData4ErListBoth.get(i));
                if (!StringUtils.equals(data1, data2)) {
                    Assert.fail("元素内容不相同 " + i);
                }
            }
        }
    }
}
