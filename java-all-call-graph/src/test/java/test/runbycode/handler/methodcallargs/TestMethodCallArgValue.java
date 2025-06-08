package test.runbycode.handler.methodcallargs;

import com.adrninistrator.jacg.handler.dto.methodcall.MethodCallWithValueSupportEnum;
import com.adrninistrator.jacg.handler.methodcall.FindMethodCallInfoHandler;
import com.adrninistrator.javacg2.util.JavaCG2ClassMethodUtil;
import org.junit.Test;
import test.callgraph.methodcallarg.util.MCAUtil;
import test.runbycode.base.TestRunByCodeBase;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2025/2/21
 * @description: 支持识别方法调用中被调用对象与参数使用的值（支持枚举）
 */
public class TestMethodCallArgValue extends TestRunByCodeBase {

    @Test
    public void $test0WriteDb() {
        commonWriteDb();
    }

    @Test
    public void test1a() {
        try (FindMethodCallInfoHandler findMethodCallInfoHandler = new FindMethodCallInfoHandler(configureWrapper)) {
            List<MethodCallWithValueSupportEnum> list = findMethodCallInfoHandler.queryMethodCallWithValueByClassMethodSupportEnum(MCAUtil.class.getName(), "run");
            printListContent(list);
        }
    }

    @Test
    public void test1b() {
        try (FindMethodCallInfoHandler findMethodCallInfoHandler = new FindMethodCallInfoHandler(configureWrapper)) {
            List<MethodCallWithValueSupportEnum> list = findMethodCallInfoHandler.queryMethodCallWithValueByClassMethodSupportEnum(MCAUtil.class.getName(), "run2");
            printListContent(list);
        }
    }

    @Test
    public void test2() {
        try (FindMethodCallInfoHandler findMethodCallInfoHandler = new FindMethodCallInfoHandler(configureWrapper)) {
            String fullMethod = JavaCG2ClassMethodUtil.formatFullMethod(MCAUtil.class.getName(), "run", String.class, int.class, Object[].class);
            List<MethodCallWithValueSupportEnum> list = findMethodCallInfoHandler.queryMethodCallWithValueByMethodSupportEnum(fullMethod, null);
            printListContent(list);
        }
    }

    @Test
    public void test3() {
        try (FindMethodCallInfoHandler findMethodCallInfoHandler = new FindMethodCallInfoHandler(configureWrapper)) {
            List<MethodCallWithValueSupportEnum> list = findMethodCallInfoHandler.queryMethodCallWithValueByClassMethodSupportEnum(MCAUtil.class.getName(), "run", 1);
            printListContent(list);
        }
    }

    @Test
    public void test4() {
        try (FindMethodCallInfoHandler findMethodCallInfoHandler = new FindMethodCallInfoHandler(configureWrapper)) {
            String fullMethod = JavaCG2ClassMethodUtil.formatFullMethod(MCAUtil.class.getName(), "run", String.class, int.class, Object[].class);
            List<MethodCallWithValueSupportEnum> list = findMethodCallInfoHandler.queryMethodCallWithValueByMethodSupportEnum(fullMethod, null, 1);
            printListContent(list);
        }
    }
}
