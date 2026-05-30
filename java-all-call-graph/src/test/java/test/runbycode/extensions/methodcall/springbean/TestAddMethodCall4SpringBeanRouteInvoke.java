package test.runbycode.extensions.methodcall.springbean;

import com.adrninistrator.jacg.conf.enums.OtherConfigFileUseListEnum;
import com.adrninistrator.jacg.conf.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.jacg.el.enums.ElConfigEnum;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Callee;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Caller;
import com.adrninistrator.jacg.runner.RunnerWriteDb;
import com.adrninistrator.javacg2.el.enums.CommonElAllowedVariableEnum;
import org.junit.Assert;
import org.junit.Test;
import test.callgraph.spring.custommethodcall.TestController;
import test.callgraph.spring.custommethodcall.router.RouteClazz;
import test.runbycode.base.TestRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2025/5/25
 * @description: 验证 Spring Bean 方法调用跳转扩展
 * 当找到 RouteClazz.invoke() 方法调用时，获得对应被调用对象的构造函数中的参数1的值（Spring Bean名称），
 * 通过该值获得对应的Spring Bean对应的类名进行替换，得到调用对应类的.execute()方法的调用关系
 */
public class TestAddMethodCall4SpringBeanRouteInvoke extends TestRunByCodeBase {

    @Test
    public void $test0RunnerWriteDb() {
        configureWrapper.setOtherConfigList(OtherConfigFileUseListEnum.OCFULE_EXTENSIONS_JAVACG2_METHOD_CALL,
                JavaCG2SpringBeanRouteInvokeMCE.class.getName());
        configureWrapper.setOtherConfigList(OtherConfigFileUseListEnum.OCFULE_EXTENSIONS_JACG_METHOD_CALL,
                JACGSpringBeanRouteInvokeMCE.class.getName());
        Assert.assertTrue(new RunnerWriteDb(javaCG2ConfigureWrapper, configureWrapper).run());
    }

    @Test
    public void testRunnerGenAllGraph4Caller() {
        // 忽略 RouteClazz.invoke() 方法被调用的情况（扩展已替换为 ImpClazzA.execute()）
        configureWrapper.setElConfigText(ElConfigEnum.ECE_GEN_ALL_CALL_GRAPH_IGNORE_METHOD_CALL,
                CommonElAllowedVariableEnum.EAVE_MC_EE_FULL_METHOD.getVariableName() + " == '" + RouteClazz.class.getName() + ":invoke()'");

        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLER,
                TestController.class.getName());
        Assert.assertTrue(new RunnerGenAllGraph4Caller(configureWrapper).run());
    }

    @Test
    public void testRunnerGenAllGraph4Callee() {
        // 忽略 RouteClazz.invoke() 方法被调用的情况（扩展已替换为 ImpClazzA.execute()）
        configureWrapper.setElConfigText(ElConfigEnum.ECE_GEN_ALL_CALL_GRAPH_IGNORE_METHOD_CALL,
                CommonElAllowedVariableEnum.EAVE_MC_EE_FULL_METHOD.getVariableName() + " == '" + RouteClazz.class.getName() + ":invoke()'");
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLEE,
                Math.class.getName() + ":sqrt");
        Assert.assertTrue(new RunnerGenAllGraph4Callee(configureWrapper).run());
    }
}
