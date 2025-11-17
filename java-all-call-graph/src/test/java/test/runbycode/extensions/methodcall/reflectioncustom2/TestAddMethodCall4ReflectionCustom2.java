package test.runbycode.extensions.methodcall.reflectioncustom2;

import com.adrninistrator.jacg.conf.enums.OtherConfigFileUseListEnum;
import com.adrninistrator.jacg.conf.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Callee;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Caller;
import com.adrninistrator.jacg.runner.RunnerWriteDb;
import org.junit.Assert;
import org.junit.Test;
import test.callgraph.reflectioncustom.methodcall.TestRunByReflectionCustom2;
import test.runbycode.base.TestRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2025/11/8
 * @description: 为通过反射工具类方法调用的被调用方法添加调用关系
 * 通过指定方法的调用参数添加方法调用，使用被调用对象，及被调用方法名
 * 对应的反射工具类方法为 test.callgraph.reflectioncustom.util.TestReflectionCustomUtil2:runByReflection(java.lang.String, java.lang.Object, java.lang.String, java.lang.Object...)
 */
public class TestAddMethodCall4ReflectionCustom2 extends TestRunByCodeBase {

    @Test
    public void $test0RunnerWriteDb() {
        configureWrapper.setOtherConfigList(OtherConfigFileUseListEnum.OCFULE_EXTENSIONS_JAVACG2_METHOD_CALL,
                JavaCG2ReflectionCustom2MethodCallExtension.class.getName());
        configureWrapper.setOtherConfigList(OtherConfigFileUseListEnum.OCFULE_EXTENSIONS_JACG_METHOD_CALL,
                JACGReflectionCustom2MethodCallExtension.class.getName());
        Assert.assertTrue(new RunnerWriteDb(javaCG2ConfigureWrapper, configureWrapper).run());
    }

    @Test
    public void testRunnerGenAllGraph4Caller() {
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLER,
                TestRunByReflectionCustom2.class.getName());
        Assert.assertTrue(new RunnerGenAllGraph4Caller(configureWrapper).run());
    }

    @Test
    public void testRunnerGenAllGraph4Callee() {
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLEE,
                System.class.getName());
        Assert.assertTrue(new RunnerGenAllGraph4Callee(configureWrapper).run());
    }
}
