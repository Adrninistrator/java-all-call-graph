package test.runbycode.callgraph;

import com.adrninistrator.jacg.conf.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.jacg.handler.method.MethodInfoHandler;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Caller;
import com.adrninistrator.javacg2.util.JavaCG2ClassMethodUtil;
import org.junit.Assert;
import org.junit.Test;
import test.annotation.JACGExample;
import test.callgraph.methodcall.TestMCCaller;
import test.runbycode.base.TestRunByCodeBase;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2024/8/24
 * @description:
 */
@JACGExample(title = "为指定包中的全部方法生成完整调用链",
        desc = {"首先查询指定包中的类的全部方法", "再为这些方法生成向下的方法完整调用链"})
public class TestGenAllGraph4CallerByPackage extends TestRunByCodeBase {

    @Test
    public void $test0WriteDb() {
        commonWriteDbForce();
    }

    @Test
    public void test() {
        try (MethodInfoHandler methodInfoHandler = new MethodInfoHandler(configureWrapper)) {
            String packageName = JavaCG2ClassMethodUtil.getPackageName(TestMCCaller.class.getName());
            List<String> fullMethodList = methodInfoHandler.queryMethodByClassNamePrefix(packageName);
            configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLER, fullMethodList.toArray(new String[]{}));
            Assert.assertTrue(new RunnerGenAllGraph4Caller(configureWrapper).run());
        }
    }
}
