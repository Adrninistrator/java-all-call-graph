package test.runbycode.reflections;

import com.adrninistrator.jacg.conf.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.conf.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.jacg.dto.methodcall.MethodCallLineData4Ee;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Callee;
import com.adrninistrator.javacg2.util.JavaCG2ClassMethodUtil;
import org.apache.commons.collections4.MapUtils;
import org.junit.Assert;
import org.junit.Test;
import test.callgraph.reflections.TestReflectedTarget;
import test.callgraph.reflections.TestReflectionCaller;
import test.runbycode.base.TestRunByCodeBase;

import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2026/2/8
 * @description:
 */
public class TestReflectionsRunnerGenAllGraph4Callee extends TestRunByCodeBase {

    @Test
    public void testWriteToFile() {
        generate(true);
    }

    @Test
    public void testReturnInMemory() {
        Map<String, List<MethodCallLineData4Ee>> methodCallLineData4EeMap = generate(false);
        Assert.assertFalse(MapUtils.isEmpty(methodCallLineData4EeMap));

        // 检查无参数方法 sayHello()
        checkMethodCaller(methodCallLineData4EeMap, "sayHello", null, String.class.getName(), "test");

        // 检查一个参数方法 greet(String)
        checkMethodCaller(methodCallLineData4EeMap, "greet", new String[]{String.class.getName()}, String.class.getName(), "test");

        // 检查两个int参数方法 add(int, int)
        checkMethodCaller(methodCallLineData4EeMap, "add", new String[]{int.class.getName(), int.class.getName()}, int.class.getName(), "test2");

        // 检查三个int参数方法 add(int, int, int)
        checkMethodCaller(methodCallLineData4EeMap, "add", new String[]{int.class.getName(), int.class.getName(), int.class.getName()}, int.class.getName(), "test");

        // 检查两个double参数方法 add(double, double)
        checkMethodCaller(methodCallLineData4EeMap, "add", new String[]{double.class.getName(), double.class.getName()}, double.class.getName(), "test");

        // 检查两个String参数方法 add(String, String)
        checkMethodCaller(methodCallLineData4EeMap, "add", new String[]{String.class.getName(), String.class.getName()}, String.class.getName(), "test");

        // 检查int + String参数方法 add(int, String)
        checkMethodCaller(methodCallLineData4EeMap, "add", new String[]{int.class.getName(), String.class.getName()}, String.class.getName(), "test");

        // 检查int + double参数方法 add(int, double)
        checkMethodCaller(methodCallLineData4EeMap, "add", new String[]{int.class.getName(), double.class.getName()}, double.class.getName(), "test");

        // 检查int + Object + double参数方法 process(int, Object, double)
        checkMethodCaller(methodCallLineData4EeMap, "process", new String[]{int.class.getName(), Object.class.getName(), double.class.getName()}, String.class.getName(), "test4");
    }

    /**
     * 检查反射调用方法的向上调用链是否包含指定的调用方法
     *
     * @param methodCallLineData4EeMap 向上调用链数据
     * @param methodName               被反射调用的方法名
     * @param argTypes                 被反射调用的方法参数类型数组，null表示无参数
     * @param returnType               被反射调用的方法返回类型
     * @param callerMethodName         调用方法名
     */
    private void checkMethodCaller(Map<String, List<MethodCallLineData4Ee>> methodCallLineData4EeMap,
                                   String methodName, String[] argTypes, String returnType, String callerMethodName) {
        String fullMethod;
        if (argTypes == null || argTypes.length == 0) {
            fullMethod = JavaCG2ClassMethodUtil.formatFullMethodStr(TestReflectedTarget.class.getName(), methodName);
        } else {
            fullMethod = JavaCG2ClassMethodUtil.formatFullMethodStr(TestReflectedTarget.class.getName(), methodName, argTypes);
        }
        String fullMethodWithReturn = JavaCG2ClassMethodUtil.genFullMethodWithReturnType(fullMethod, returnType);
        List<MethodCallLineData4Ee> methodCallLineData4EeList = methodCallLineData4EeMap.get(fullMethodWithReturn);
        Assert.assertNotNull("未找到方法: " + fullMethodWithReturn, methodCallLineData4EeList);
        String testMethod = JavaCG2ClassMethodUtil.formatFullMethodStr(TestReflectionCaller.class.getName(), callerMethodName);
        checkCalleeGraphContainsCaller(methodCallLineData4EeList, testMethod);
    }

    private Map<String, List<MethodCallLineData4Ee>> generate(boolean writeToFile) {
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLEE,
                TestReflectedTarget.class.getName());

        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_WRITE_TO_FILE, String.valueOf(writeToFile));
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_RETURN_IN_MEMORY, String.valueOf(!writeToFile));

        RunnerGenAllGraph4Callee runnerGenAllGraph4Callee = new RunnerGenAllGraph4Callee(configureWrapper);
        Assert.assertTrue(runnerGenAllGraph4Callee.run());
        if (writeToFile) {
            return null;
        }
        return runnerGenAllGraph4Callee.getAllMethodCallLineData4EeMap();
    }
}
