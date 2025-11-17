package test.runbycode.base;

import com.adrninistrator.jacg.compatibility.runner.RunnerJarCompatibilityCheckFast;
import com.adrninistrator.jacg.conf.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.conf.enums.OtherConfigFileUseListEnum;
import com.adrninistrator.jacg.conf.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.jacg.dto.methodcall.MethodCallLineData4Ee;
import com.adrninistrator.jacg.dto.methodcall.MethodCallLineData4Er;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4SpringAopAdviceAffectedMethod;
import com.adrninistrator.jacg.el.enums.ElConfigEnum;
import com.adrninistrator.jacg.handler.spring.SpringHandler;
import com.adrninistrator.jacg.jardiff.dto.method.ModifiedMethodInfo;
import com.adrninistrator.jacg.jardiff.runner.AbstractRunnerGenJarDiffCallGraph;
import com.adrninistrator.jacg.jardiff.runner.RunnerGenJarDiffCalleeGraph;
import com.adrninistrator.jacg.jardiff.runner.RunnerGenJarDiffCallerGraph;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.junit.Assert;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import test.callgraph.elexample.callee.TestElExampleCallee1;
import test.callgraph.elexample.caller.TestElExampleCaller1;
import test.callgraph.elexample.caller.TestElExampleCaller2;
import test.runbycode.jardiffcallgraph.TestRunnerGenJarDiffCallGraphDiffSame;

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2025/9/12
 * @description: 测试EL表达式基类
 */
public abstract class TestElRunByCodeBase extends TestRunByCodeBase {

    private static final Logger logger = LoggerFactory.getLogger(TestElRunByCodeBase.class);

    public final String METHOD_EXAMPLE_CALLER_2_TEST1 = TestElExampleCaller2.class.getName() + ":test1()";
    public final String METHOD_EXAMPLE_CALLER_2_TEST3 = TestElExampleCaller2.class.getName() + ":test3()";

    /**
     * 选择需要测试的EL表达式枚举常量
     *
     * @return
     */
    protected abstract ElConfigEnum chooseElConfigEnum();

    /**
     * 选择需要使用的EL表达式文本
     *
     * @return
     */
    protected abstract String chooseElText();

    /**
     * 选择当前测试类显示的标题
     *
     * @return
     */
    protected abstract String chooseTitle();

    /**
     * 选择当前测试类的描述
     *
     * @return
     */
    protected abstract String chooseDesc();

    /**
     * 返回当前示例是否为字符串比较
     *
     * @return
     */
    protected boolean example4StringCompare() {
        return false;
    }

    /**
     * 设置EL表达式配置参数
     */
    protected void setConfig4ElText() {
        ElConfigEnum elConfigEnum = chooseElConfigEnum();
        String elText = chooseElText();
        configureWrapper.setElConfigText(elConfigEnum, elText);
    }

    /**
     * 生成向上的方法完整调用链，仅指定不超过一个方法
     */
    protected List<MethodCallLineData4Ee> genElOneCalleeGraph(String... methodClasses4Callee) {
        return genElOneCalleeGraph(1, methodClasses4Callee);
    }

    /**
     * 生成向上的方法完整调用链，仅指定不超过一个方法
     */
    protected List<MethodCallLineData4Ee> genElOneCalleeGraph(int expectedNum, String... methodClasses4Callee) {
        setConfig4ElText();
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_RETURN_IN_MEMORY, Boolean.TRUE.toString());
        if (ArrayUtils.isNotEmpty(methodClasses4Callee)) {
            Assert.assertEquals(1, methodClasses4Callee.length);
            configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLEE,
                    methodClasses4Callee);
        } else {
            configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLEE,
                    TestElExampleCallee1.class.getName() + ":testC()");
        }
        return genOneCalleeGraph(expectedNum);
    }

    protected void checkCalleeGraph(List<MethodCallLineData4Ee> methodCallLineData4EeList, String... expectedFullMethods) {
        if (ArrayUtils.isEmpty(expectedFullMethods)) {
            Assert.assertTrue(JavaCG2Util.isCollectionEmpty(methodCallLineData4EeList));
            return;
        }
        Assert.assertEquals(expectedFullMethods.length, methodCallLineData4EeList.size());
        for (int i = 0; i < expectedFullMethods.length; i++) {
            Assert.assertTrue(StringUtils.equals(expectedFullMethods[i], methodCallLineData4EeList.get(i).getActualFullMethod()));
        }
    }

    protected void genCalleeGraphAndCheckNoEr() {
        List<MethodCallLineData4Ee> methodCallLineData4EeList = genElOneCalleeGraph();
        checkCalleeGraph(methodCallLineData4EeList,
                TestElExampleCallee1.class.getName() + ":testC()",
                TestElExampleCallee1.class.getName() + ":testB()",
                TestElExampleCallee1.class.getName() + ":testA()");
    }

    protected void genCalleeGraphAndCheckNoErEe() {
        List<MethodCallLineData4Ee> methodCallLineData4EeList = genElOneCalleeGraph();
        checkCalleeGraph(methodCallLineData4EeList,
                TestElExampleCallee1.class.getName() + ":testC()");
    }

    protected void runCompatibilityCheckFast(boolean skipWriteDb) {
        setConfig4ElText();
        RunnerJarCompatibilityCheckFast runnerJarCompatibilityCheckFast = new RunnerJarCompatibilityCheckFast(javaCG2ConfigureWrapper, configureWrapper);
        runnerJarCompatibilityCheckFast.setSkipWriteDb(skipWriteDb);
        Assert.assertTrue(runnerJarCompatibilityCheckFast.run());
    }

    /**
     * 生成向下的方法完整调用链，仅指定不超过一个方法
     */
    protected List<MethodCallLineData4Er> genElOneCallerGraph(String... methodClasses4Caller) {
        return genElOneCallerGraph(1, methodClasses4Caller);
    }

    /**
     * 生成向下的方法完整调用链，仅指定不超过一个方法
     */
    protected List<MethodCallLineData4Er> genElOneCallerGraph(int expectedNum, String... methodClasses4Caller) {
        setConfig4ElText();
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_RETURN_IN_MEMORY, Boolean.TRUE.toString());
        if (ArrayUtils.isNotEmpty(methodClasses4Caller)) {
            Assert.assertEquals(1, methodClasses4Caller.length);
            configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLER,
                    methodClasses4Caller);
        } else {
            configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLER,
                    TestElExampleCaller1.class.getName() + ":test1(");
        }

        return genOneCallerGraph(expectedNum);
    }

    protected void checkCallerGraph(List<MethodCallLineData4Er> methodCallLineData4ErList, String... expectedFullMethods) {
        if (ArrayUtils.isEmpty(expectedFullMethods)) {
            Assert.assertTrue(JavaCG2Util.isCollectionEmpty(methodCallLineData4ErList));
            return;
        }
        Assert.assertEquals(expectedFullMethods.length, methodCallLineData4ErList.size());
        for (int i = 0; i < expectedFullMethods.length; i++) {
            Assert.assertEquals(expectedFullMethods[i], methodCallLineData4ErList.get(i).getActualFullMethod());
        }
    }

    protected void genCallerGraphAndCheckNoEe() {
        List<MethodCallLineData4Er> methodCallLineData4ErList = genElOneCallerGraph();
        checkCallerGraph(methodCallLineData4ErList,
                TestElExampleCaller1.class.getName() + ":test1(" + String.class.getName() + ")");
    }

    protected void writeDbSupportSpringAop(String springBeanClassName, boolean contains) {
        setConfig4ElText();
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_PARSE_SPRING_AOP_INFO, Boolean.TRUE.toString());

        commonWriteDbForce();

        try (SpringHandler springHandler = new SpringHandler(configureWrapper)) {
            List<WriteDbData4SpringAopAdviceAffectedMethod> springAopAdviceAffectedMethodList = springHandler.querySpringAopAdviceAffectedMethodBySPC(springBeanClassName);
            Assert.assertEquals(contains, !JavaCG2Util.isCollectionEmpty(springAopAdviceAffectedMethodList));
        }
    }

    protected void genJarDiffCallGraph(boolean genCalleeGraph, String[] containsFullMethodPrefixes, String[] notContainsFullMethodPrefixes) {
        setConfig4ElText();
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_APP_NAME, TestRunnerGenJarDiffCallGraphDiffSame.APP_NAME);

        AbstractRunnerGenJarDiffCallGraph genJarDiffCallGraph;
        OtherConfigFileUseListEnum otherConfigFileUseListEnumJarDiff;
        if (genCalleeGraph) {
            genJarDiffCallGraph = new RunnerGenJarDiffCalleeGraph(javaCG2ConfigureWrapper, configureWrapper, null, null);
            otherConfigFileUseListEnumJarDiff = OtherConfigFileUseListEnum.OCFULE_JAR_DIFF_CALLEE_GRAPH_DIR;
        } else {
            genJarDiffCallGraph = new RunnerGenJarDiffCallerGraph(javaCG2ConfigureWrapper, configureWrapper);
            otherConfigFileUseListEnumJarDiff = OtherConfigFileUseListEnum.OCFULE_JAR_DIFF_CALLER_GRAPH_DIR;
        }

        configureWrapper.setOtherConfigList(otherConfigFileUseListEnumJarDiff,
                TestRunnerGenJarDiffCallGraphDiffSame.ROOT_JAR_DIR + TestRunnerGenJarDiffCallGraphDiffSame.DIR_NAME_OLD,
                TestRunnerGenJarDiffCallGraphDiffSame.ROOT_JAR_DIR + TestRunnerGenJarDiffCallGraphDiffSame.DIR_NAME_NEW);

        Assert.assertTrue(genJarDiffCallGraph.run());
        Map<String, List<ModifiedMethodInfo>> map = genJarDiffCallGraph.getJarModifiedMethodInfoMap();
        Set<String> containsFullMethodPrefixSet = new HashSet<>(Arrays.asList(containsFullMethodPrefixes));
        Set<String> foundContainsFullMethodPrefixSet = new HashSet<>();
        for (List<ModifiedMethodInfo> methodInfoList : map.values()) {
            for (ModifiedMethodInfo methodInfo : methodInfoList) {
                logger.info("找到方法 {}", methodInfo.getFullMethod());
                for (String notContainsFullMethodPrefix : notContainsFullMethodPrefixes) {
                    if (methodInfo.getFullMethod().startsWith(notContainsFullMethodPrefix)) {
                        logger.error("出现了不应该出现的方法 {} {}", notContainsFullMethodPrefix, methodInfo.getFullMethod());
                        Assert.fail();
                    }
                }
                for (String containsFullMethodPrefix : containsFullMethodPrefixes) {
                    if (methodInfo.getFullMethod().startsWith(containsFullMethodPrefix)) {
                        foundContainsFullMethodPrefixSet.add(containsFullMethodPrefix);
                    }
                }
            }
        }
        for (String containsFullMethodPrefix : containsFullMethodPrefixSet) {
            if (!foundContainsFullMethodPrefixSet.contains(containsFullMethodPrefix)) {
                logger.error("未找到应该出现的方法前缀 {}", containsFullMethodPrefix);
                Assert.fail();
            }
        }
    }
}