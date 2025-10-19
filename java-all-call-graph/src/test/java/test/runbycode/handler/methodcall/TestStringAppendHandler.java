package test.runbycode.handler.methodcall;

import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCall;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodInfo;
import com.adrninistrator.jacg.handler.dto.string.StringAppendParseResult;
import com.adrninistrator.jacg.handler.method.MethodInfoHandler;
import com.adrninistrator.jacg.handler.methodcall.MethodCallHandler;
import com.adrninistrator.jacg.handler.methodcall.StringAppendHandler;
import com.adrninistrator.javacg2.common.JavaCG2CommonNameConstants;
import org.apache.commons.lang3.StringUtils;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import test.callgraph.enums.DbStatementEnum;
import test.callgraph.stringappend.argument.TestStringAppendArgClassName;
import test.callgraph.stringappend.argument.TestStringAppendArgOnlyConstants;
import test.callgraph.stringappend.argument.TestStringAppendArgOther;
import test.callgraph.stringappend.argument.TestStringAppendArgSimpleClassName;
import test.callgraph.stringappend.argument.TestStringAppendArgSupportEnumMCR;
import test.callgraph.stringappend.argument.TestStringAppendArgSupportEnumMCR2;
import test.callgraph.stringappend.argument.TestStringPlusArgSupportEnumMCR;
import test.callgraph.stringappend.argument.TestStringPlusArgSupportEnumMCR2;
import test.callgraph.stringappend.methodreturn.TestStringAppendReturnClassName;
import test.callgraph.stringappend.methodreturn.TestStringAppendReturnOnlyConstants;
import test.callgraph.stringappend.methodreturn.TestStringAppendReturnOther;
import test.callgraph.stringappend.methodreturn.TestStringAppendReturnSimpleClassName;
import test.callgraph.stringappend.methodreturn.TestStringAppendReturnSupportEnumMCR;
import test.callgraph.stringappend.methodreturn.TestStringPlusReturnSupportEnumMCR;
import test.runbycode.base.TestRunByCodeBase;

import java.io.PrintStream;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2025/9/2
 * @description:
 */
public class TestStringAppendHandler extends TestRunByCodeBase {

    private static final Logger logger = LoggerFactory.getLogger(TestStringAppendHandler.class);

    public static final String EXPECTED_VALUE = "abcd";
    public static final String EXPECTED_VALUE2 = "ab" + DbStatementEnum.DSE_UPDATE.getStatement() + "cd";
    public static final String EXPECTED_VALUE3 = "ab" + DbStatementEnum.DSE_UPDATE.name() + "cd";

    private StringAppendHandler stringAppendHandler;
    private MethodInfoHandler methodInfoHandler;
    private MethodCallHandler methodCallHandler;

    @Before
    public void init() {
        stringAppendHandler = new StringAppendHandler(configureWrapper);
        methodInfoHandler = new MethodInfoHandler(configureWrapper);
        methodCallHandler = new MethodCallHandler(configureWrapper);
    }

    @After
    public void after() {
        stringAppendHandler.close();
        methodInfoHandler.close();
        methodCallHandler.close();
    }

    @Test
    public void $test0WriteDb() {
        commonWriteDb();
    }

    @Test
    public void testArg1() {
        doTest(TestStringAppendArgOnlyConstants.class.getName(), true, EXPECTED_VALUE);
    }

    @Test
    public void testArg2A() {
        doTest(TestStringAppendArgSupportEnumMCR.class.getName(), true, EXPECTED_VALUE2);
    }

    @Test
    public void testArg2B() {
        doTest(TestStringAppendArgSupportEnumMCR2.class.getName(), true, EXPECTED_VALUE3);
    }

    @Test
    public void testArg3A() {
        doTest(TestStringPlusArgSupportEnumMCR.class.getName(), true, EXPECTED_VALUE2);
    }

    @Test
    public void testArg3B() {
        doTest(TestStringPlusArgSupportEnumMCR2.class.getName(), true, EXPECTED_VALUE3);
    }

    @Test
    public void testArg4() {
        doTest(TestStringAppendArgOther.class.getName(), true, EXPECTED_VALUE);
    }

    @Test
    public void testArg5() {
        doTest(TestStringAppendArgClassName.class.getName(), true, TestStringAppendArgClassName.class.getName());
    }

    @Test
    public void testArg6() {
        doTest(TestStringAppendArgSimpleClassName.class.getName(), true, TestStringAppendArgSimpleClassName.class.getSimpleName());
    }

    @Test
    public void testReturn1() {
        doTest(TestStringAppendReturnOnlyConstants.class.getName(), false, EXPECTED_VALUE);
    }

    @Test
    public void testReturn2() {
        doTest(TestStringAppendReturnOther.class.getName(), false, EXPECTED_VALUE);
    }

    @Test
    public void testReturn3A() {
        doTest(TestStringAppendReturnSupportEnumMCR.class.getName(), false, EXPECTED_VALUE2);
    }

    @Test
    public void testReturn3B() {
        doTest(TestStringAppendReturnSupportEnumMCR.class.getName(), false, EXPECTED_VALUE2);
    }

    @Test
    public void testReturn4A() {
        doTest(TestStringPlusReturnSupportEnumMCR.class.getName(), false, EXPECTED_VALUE2);
    }

    @Test
    public void testReturn4B() {
        doTest(TestStringPlusReturnSupportEnumMCR.class.getName(), false, EXPECTED_VALUE2);
    }

    @Test
    public void testReturn5() {
        doTest(TestStringAppendReturnClassName.class.getName(), false, TestStringAppendReturnClassName.class.getName());
    }

    @Test
    public void testReturn6() {
        doTest(TestStringAppendReturnSimpleClassName.class.getName(), false, TestStringAppendReturnSimpleClassName.class.getSimpleName());
    }

    public void doTest(String callerClassName, boolean parseArg, String expectedValue) {
        List<WriteDbData4MethodInfo> methodInfoList = methodInfoHandler.queryMethodInfoByClass(callerClassName);
        for (WriteDbData4MethodInfo methodInfo : methodInfoList) {
            if (JavaCG2CommonNameConstants.METHOD_NAME_INIT.equals(methodInfo.getMethodName())) {
                continue;
            }
            if (parseArg) {
                List<WriteDbData4MethodCall> methodCallList = methodCallHandler.queryByErHashEeClassesMethod(methodInfo.getMethodHash(), "println", PrintStream.class.getName());
                for (WriteDbData4MethodCall methodCall : methodCallList) {
                    StringAppendParseResult stringAppendParseResult = stringAppendHandler.parseStringAppend4MethodArg(methodCall.getCallId(), 1);
                    checkResult(stringAppendParseResult, methodInfo, expectedValue);
                }
                continue;
            }
            if (!JavaCG2CommonNameConstants.CLASS_NAME_STRING.equals(methodInfo.getReturnType())) {
                continue;
            }
            StringAppendParseResult stringAppendParseResult = stringAppendHandler.parseStringAppend4MethodReturn(methodInfo.getFullMethod(), methodInfo.getReturnType());
            checkResult(stringAppendParseResult, methodInfo, expectedValue);
        }
    }

    private void checkResult(StringAppendParseResult stringAppendParseResult, WriteDbData4MethodInfo methodInfo, String expectedValue) {
        if (StringUtils.isEmpty(stringAppendParseResult.getRawString())) {
            logger.error("### 解析结果为空 {}", methodInfo.getFullMethod());
            Assert.fail("解析结果为空");
            return;
        }
        logger.info("### 解析结果如下 [{}] [{}] [{}]", methodInfo.getFullMethod(), stringAppendParseResult.getRawString(),
                stringAppendParseResult.getParsedValue());
        if (!StringUtils.equals(stringAppendParseResult.getParsedValue(), expectedValue)) {
            logger.error("解析结果与预期不一致 [{}] [{}]", stringAppendParseResult.getParsedValue(), expectedValue);
            Assert.fail("解析结果与预期不一致");
        }
    }
}
