package test.runbycode.handler.methodcallargs;

import com.adrninistrator.javacg2.common.JavaCG2Constants;
import org.junit.Assert;
import org.junit.Test;
import test.callgraph.reflectioncustom.util.TestReflectionCustomUtil1;
import test.callgraph.reflectioncustom.util.TestReflectionCustomUtilWrapper1;
import test.runbycode.base.TestRunByCodeBase;
import test.runbycode.handler.methodcallargs.handler.TestReflectionUtil1MethodCallArgInfoRecordHandler;

import java.util.Arrays;

/**
 * @author adrninistrator
 * @date 2025/10/11
 * @description: 测试获取指定方法被调用时使用的参数类型与值等信息，并记录到文件
 */
public class TestMethodCallArgInfoRecordHandler extends TestRunByCodeBase {

    @Test
    public void $test0WriteDb() {
        commonWriteDb();
    }

    @Test
    public void testReflectionUtil1() {
        try (TestReflectionUtil1MethodCallArgInfoRecordHandler testReflectionUtil1MethodCallArgInfoRecordHandler =
                     new TestReflectionUtil1MethodCallArgInfoRecordHandler(configureWrapper,
                             Arrays.asList(TestReflectionCustomUtil1.class.getName(), TestReflectionCustomUtilWrapper1.class.getName()),
                             "build/" + TestReflectionUtil1MethodCallArgInfoRecordHandler.class.getSimpleName() + JavaCG2Constants.EXT_MD,
                             true
                     )) {
            Assert.assertTrue(testReflectionUtil1MethodCallArgInfoRecordHandler.run());
        }
    }
}
