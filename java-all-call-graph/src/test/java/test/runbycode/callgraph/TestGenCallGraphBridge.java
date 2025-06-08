package test.runbycode.callgraph;

import com.adrninistrator.jacg.common.enums.OutputDetailEnum;
import com.adrninistrator.jacg.conf.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.conf.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.jacg.dto.methodcall.MethodCallLineData4Ee;
import com.adrninistrator.jacg.dto.methodcall.MethodCallLineData4Er;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Callee;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Caller;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import test.callgraph.interfacesgeneric.classes.GenericAbstractSuper2;
import test.callgraph.interfacesgeneric.classes.GenericClassImplSuper2b2;
import test.runbycode.base.TestRunByCodeBase;

import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2025/4/9
 * @description:
 */
public class TestGenCallGraphBridge extends TestRunByCodeBase {

    @Before
    public void init() {
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_OUTPUT_DETAIL, OutputDetailEnum.ODE_0.getDetail());
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_RETURN_IN_MEMORY, Boolean.TRUE.toString());
    }

    @Test
    public void testGenCallGraph4ee() {
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLEE,
                GenericClassImplSuper2b2.class.getName() + ":doTest()"
        );
        RunnerGenAllGraph4Callee runnerGenAllGraph4Callee = new RunnerGenAllGraph4Callee(configureWrapper);
        Assert.assertTrue(runnerGenAllGraph4Callee.run());
        Map<String, List<MethodCallLineData4Ee>> allMethodCallLineData4EeMap = runnerGenAllGraph4Callee.getAllMethodCallLineData4EeMap();
        boolean found = false;
        for (Map.Entry<String, List<MethodCallLineData4Ee>> entry : allMethodCallLineData4EeMap.entrySet()) {
            List<MethodCallLineData4Ee> methodCallLineData4EeList = entry.getValue();
            Assert.assertFalse(JavaCG2Util.isCollectionEmpty(methodCallLineData4EeList));
            for (MethodCallLineData4Ee methodCallLineData4Ee : methodCallLineData4EeList) {
                if (methodCallLineData4Ee.getActualFullMethod().startsWith(GenericAbstractSuper2.class.getName() + ":test(")) {
                    found = true;
                    break;
                }
            }
        }
        Assert.assertTrue(found);
    }

    @Test
    public void testGenCallGraph4er() {
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLER,
                GenericClassImplSuper2b2.class.getName() + ":test(java.lang.Object,java.lang.Object,java.lang.String,int)"
        );
        RunnerGenAllGraph4Caller runnerGenAllGraph4Caller = new RunnerGenAllGraph4Caller(configureWrapper);
        Assert.assertTrue(runnerGenAllGraph4Caller.run());
        Map<String, List<MethodCallLineData4Er>> allMethodCallLineData4ErMap = runnerGenAllGraph4Caller.getAllMethodCallLineData4ErMap();
        Assert.assertFalse(JavaCG2Util.isMapEmpty(allMethodCallLineData4ErMap));
        boolean found = false;
        for (Map.Entry<String, List<MethodCallLineData4Er>> entry : allMethodCallLineData4ErMap.entrySet()) {
            List<MethodCallLineData4Er> methodCallLineData4ErList = entry.getValue();
            for (MethodCallLineData4Er methodCallLineData4Er : methodCallLineData4ErList) {
                if (methodCallLineData4Er.getActualFullMethod().startsWith(GenericClassImplSuper2b2.class.getName() + ":doTest()")) {
                    found = true;
                    break;
                }
            }
        }
        Assert.assertTrue(found);
    }
}
