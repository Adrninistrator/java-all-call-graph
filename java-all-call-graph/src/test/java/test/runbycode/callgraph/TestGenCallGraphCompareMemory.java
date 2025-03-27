package test.runbycode.callgraph;

import com.adrninistrator.jacg.dto.methodcall.MethodCallLineData4Ee;
import com.adrninistrator.jacg.dto.methodcall.MethodCallLineData4Er;
import com.adrninistrator.jacg.util.JACGJsonUtil;
import org.apache.commons.lang3.StringUtils;
import org.junit.Assert;
import org.junit.Test;
import test.runbycode.base.TestRunByCodeBase;
import test.runbycodemain.TestRBCRunnerGenAllGraph4Callee;
import test.runbycodemain.TestRBCRunnerGenAllGraph4Caller;

import java.util.List;

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
        List<MethodCallLineData4Ee> methodCallLineData4EeListInMemory = testRBCRunnerGenAllGraph4Callee.doTestReturnInMemory();
        List<MethodCallLineData4Ee> methodCallLineData4EeListBoth = testRBCRunnerGenAllGraph4Callee.doTestBoth();
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

    @Test
    public void testCompareEr() {
        testRBCRunnerGenAllGraph4Caller.setConfigureWrapper(configureWrapper);
        List<MethodCallLineData4Er> methodCallLineData4ErListInMemory = testRBCRunnerGenAllGraph4Caller.doTestReturnInMemory();
        List<MethodCallLineData4Er> methodCallLineData4ErListBoth = testRBCRunnerGenAllGraph4Caller.doTestBoth();
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
