package test.runbycode.extensions.methodcall.commonslang3methodutils;

import com.adrninistrator.jacg.conf.enums.OtherConfigFileUseListEnum;
import com.adrninistrator.jacg.conf.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Callee;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Caller;
import com.adrninistrator.jacg.runner.RunnerWriteDb;
import org.junit.Assert;
import org.junit.Test;
import test.callgraph.reflectioncommonslang3.invoke.TestReflectionCommonsLang3Invoke;
import test.runbycode.base.TestRunByCodeBase;

import java.io.PrintStream;

/**
 * @author adrninistrator
 * @date 2025/11/17
 * @description: 为通过反射工具类方法调用的被调用方法添加调用关系
 * 通过指定方法的调用参数添加方法调用，使用被调用对象，及被调用方法名
 * 对应的反射工具类方法为 org.apache.commons.lang3.reflect.MethodUtils:invokeMethod(java.lang.Object, java.lang.String)
 */
public class TestAddMethodCall4CommonsLang3MethodUtils extends TestRunByCodeBase {

    @Test
    public void $test0RunnerWriteDb() {
        configureWrapper.setOtherConfigList(OtherConfigFileUseListEnum.OCFULE_EXTENSIONS_JAVACG2_METHOD_CALL,
                JavaCG2CommonsLang3MethodUtilsMethodCallExtension.class.getName());
        configureWrapper.setOtherConfigList(OtherConfigFileUseListEnum.OCFULE_EXTENSIONS_JACG_METHOD_CALL,
                JACGCommonsLang3MethodUtilsMethodCallExtension.class.getName());
        Assert.assertTrue(new RunnerWriteDb(javaCG2ConfigureWrapper, configureWrapper).run());
    }

    @Test
    public void testRunnerGenAllGraph4Caller() {
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLER,
                TestReflectionCommonsLang3Invoke.class.getName());
        Assert.assertTrue(new RunnerGenAllGraph4Caller(configureWrapper).run());
    }

    @Test
    public void testRunnerGenAllGraph4Callee() {
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLEE,
                PrintStream.class.getName());
        Assert.assertTrue(new RunnerGenAllGraph4Callee(configureWrapper).run());
    }
}
