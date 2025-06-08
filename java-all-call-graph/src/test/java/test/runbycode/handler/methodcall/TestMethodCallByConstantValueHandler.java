package test.runbycode.handler.methodcall;

import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCallInfo;
import com.adrninistrator.jacg.handler.dto.methodcall.MethodCallWithInfo;
import com.adrninistrator.jacg.handler.methodcall.FindMethodCallByCallInfoHandler;
import com.adrninistrator.javacg2.common.enums.JavaCG2ConstantTypeEnum;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.junit.Assert;
import org.junit.Test;
import test.callgraph.methodargument.TestArgument3;
import test.runbycode.base.TestRunByCodeBase;

import java.io.PrintStream;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2025/5/27
 * @description:
 */
public class TestMethodCallByConstantValueHandler extends TestRunByCodeBase {

    public static final String NOT_EXISTS_VALUE = "this_is_not_used_value_202505311428";

    @Test
    public void $test0WriteDb() {
        commonWriteDb();
    }

    @Test
    public void testQueryMethodCallByConstantValue1() {
        try (FindMethodCallByCallInfoHandler methodCallByCallInfoHandler = new FindMethodCallByCallInfoHandler(configureWrapper)) {
            doTestQueryMethodCallByConstantValue1(methodCallByCallInfoHandler, true, JavaCG2ConstantTypeEnum.CONSTTE_STRING, "");
            doTestQueryMethodCallByConstantValue1(methodCallByCallInfoHandler, true, JavaCG2ConstantTypeEnum.CONSTTE_STRING, "1");
            doTestQueryMethodCallByConstantValue1(methodCallByCallInfoHandler, true, JavaCG2ConstantTypeEnum.CONSTTE_STRING, "ok");
            doTestQueryMethodCallByConstantValue1(methodCallByCallInfoHandler, true, JavaCG2ConstantTypeEnum.CONSTTE_INT, "1");

            doTestQueryMethodCallByConstantValue1(methodCallByCallInfoHandler, false, JavaCG2ConstantTypeEnum.CONSTTE_STRING, NOT_EXISTS_VALUE);
        }
    }

    @Test
    public void testQueryMethodCallByConstantValue4Method1() {
        try (FindMethodCallByCallInfoHandler methodCallByCallInfoHandler = new FindMethodCallByCallInfoHandler(configureWrapper)) {
            doTestQueryMethodCallByConstantValue4Method1(methodCallByCallInfoHandler, true, JavaCG2ConstantTypeEnum.CONSTTE_STRING, "ok", PrintStream.class.getName(), "println");
            doTestQueryMethodCallByConstantValue4Method1(methodCallByCallInfoHandler, true, JavaCG2ConstantTypeEnum.CONSTTE_INT, "1", TestArgument3.class.getName(), "test1");

            doTestQueryMethodCallByConstantValue4Method1(methodCallByCallInfoHandler, false, JavaCG2ConstantTypeEnum.CONSTTE_STRING, "ok", "a", "b");
            doTestQueryMethodCallByConstantValue4Method1(methodCallByCallInfoHandler, false, JavaCG2ConstantTypeEnum.CONSTTE_INT, "1", "a", "b");
            doTestQueryMethodCallByConstantValue4Method1(methodCallByCallInfoHandler, false, JavaCG2ConstantTypeEnum.CONSTTE_STRING, NOT_EXISTS_VALUE, "a", "b");
        }
    }

    @Test
    public void testQueryMethodCallByConstantValue4Method2() {
        try (FindMethodCallByCallInfoHandler methodCallByCallInfoHandler = new FindMethodCallByCallInfoHandler(configureWrapper)) {
            doTestQueryMethodCallByConstantValue4Method2(methodCallByCallInfoHandler, true, JavaCG2ConstantTypeEnum.CONSTTE_STRING, "ok", "java.io.PrintStream:println(java.lang" +
                    ".String)");
            doTestQueryMethodCallByConstantValue4Method2(methodCallByCallInfoHandler, true, JavaCG2ConstantTypeEnum.CONSTTE_INT, "1", "test.callgraph.methodargument" +
                    ".TestArgument3:test1" +
                    "(java.lang.Object,java.lang.String,int)");

            doTestQueryMethodCallByConstantValue4Method2(methodCallByCallInfoHandler, false, JavaCG2ConstantTypeEnum.CONSTTE_STRING, "ok", "a:b()");
            doTestQueryMethodCallByConstantValue4Method2(methodCallByCallInfoHandler, false, JavaCG2ConstantTypeEnum.CONSTTE_INT, "1", "a:b()");
            doTestQueryMethodCallByConstantValue4Method2(methodCallByCallInfoHandler, false, JavaCG2ConstantTypeEnum.CONSTTE_STRING, NOT_EXISTS_VALUE, "a:b()");
        }
    }

    private void doTestQueryMethodCallByConstantValue1(FindMethodCallByCallInfoHandler methodCallByCallInfoHandler, boolean exists,
                                                       JavaCG2ConstantTypeEnum javaCG2ConstantTypeEnum, String value) {
        List<MethodCallWithInfo<WriteDbData4MethodCallInfo>> methodCallWithInfoList = methodCallByCallInfoHandler.queryMethodCallByConstantValue(javaCG2ConstantTypeEnum, value);
        Assert.assertEquals(exists, !JavaCG2Util.isCollectionEmpty(methodCallWithInfoList));
        String flag = javaCG2ConstantTypeEnum.getType() + "@" + value;
        printListContent(methodCallWithInfoList, flag);
    }

    private void doTestQueryMethodCallByConstantValue4Method1(FindMethodCallByCallInfoHandler methodCallByCallInfoHandler, boolean exists,
                                                              JavaCG2ConstantTypeEnum javaCG2ConstantTypeEnum, String value, String calleeClassName, String calleeMethodName) {
        List<MethodCallWithInfo<WriteDbData4MethodCallInfo>> methodCallWithInfoList = methodCallByCallInfoHandler.queryMethodCallByConstantValue4Method(javaCG2ConstantTypeEnum,
                value,
                calleeClassName, calleeMethodName);
        Assert.assertEquals(exists, !JavaCG2Util.isCollectionEmpty(methodCallWithInfoList));
        String flag = javaCG2ConstantTypeEnum.getType() + "@" + value;
        printListContent(methodCallWithInfoList, flag);
    }

    private void doTestQueryMethodCallByConstantValue4Method2(FindMethodCallByCallInfoHandler methodCallByCallInfoHandler, boolean exists,
                                                              JavaCG2ConstantTypeEnum javaCG2ConstantTypeEnum, String value, String calleeFullMethod) {
        List<MethodCallWithInfo<WriteDbData4MethodCallInfo>> methodCallWithInfoList = methodCallByCallInfoHandler.queryMethodCallByConstantValue4Method(javaCG2ConstantTypeEnum,
                value,
                calleeFullMethod);
        Assert.assertEquals(exists, !JavaCG2Util.isCollectionEmpty(methodCallWithInfoList));
        String flag = javaCG2ConstantTypeEnum.getType() + "@" + value;
        printListContent(methodCallWithInfoList, flag);
    }
}
