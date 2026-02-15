package test.runbycode.reflections;

import com.adrninistrator.jacg.conf.enums.OtherConfigFileUseListEnum;
import com.adrninistrator.jacg.extensions.methodcall.JACGReflectionMethodCallExtension;
import com.adrninistrator.jacg.extensions.methodcall.JavaCG2ReflectionMethodCallExtension;
import com.adrninistrator.jacg.runner.RunnerWriteDb;
import com.adrninistrator.javacg2.conf.enums.JavaCG2OtherConfigFileUseListEnum;
import org.junit.Assert;
import org.junit.Test;
import test.runbycode.base.TestRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2026/2/8
 * @description: 验证反射方法调用解析功能 - 写数据库
 */
public class Test0ReflectionsRunnerWriteDb extends TestRunByCodeBase {

    @Test
    public void $test0WriteDb() {
        // 设置需要解析的 jar 包目录（包含 reflections 和 reflections2）
        javaCG2ConfigureWrapper.setOtherConfigList(JavaCG2OtherConfigFileUseListEnum.OCFULE_JAR_DIR,
                "out/test/classes/test/callgraph/reflections");

        // 注册扩展类
        configureWrapper.setOtherConfigList(OtherConfigFileUseListEnum.OCFULE_EXTENSIONS_JAVACG2_METHOD_CALL,
                JavaCG2ReflectionMethodCallExtension.class.getName());
        configureWrapper.setOtherConfigList(OtherConfigFileUseListEnum.OCFULE_EXTENSIONS_JACG_METHOD_CALL,
                JACGReflectionMethodCallExtension.class.getName());

        Assert.assertTrue(new RunnerWriteDb(javaCG2ConfigureWrapper, configureWrapper).run());
    }
}
