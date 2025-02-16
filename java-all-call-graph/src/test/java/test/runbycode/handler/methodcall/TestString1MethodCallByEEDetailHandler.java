package test.runbycode.handler.methodcall;

import org.apache.commons.lang3.StringUtils;
import org.junit.Assert;
import org.junit.Test;
import test.runbycode.base.TestRunByCodeBase;
import test.runbycode.handler.methodcall.handler.StringAllMethodCallByEEDetailHandler;
import test.runbycode.handler.methodcall.handler.StringAppendCharMethodCallByEEDetailHandler;

import java.util.Arrays;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/7/5
 * @description:
 */
public class TestString1MethodCallByEEDetailHandler extends TestRunByCodeBase {

    @Test
    public void $test0WriteDb() {
        commonWriteDb();
    }

    @Test
    public void test() {
        List<String> methodNameList = Arrays.asList("indexOf", "IndexOf", "substring");
        try (StringAppendCharMethodCallByEEDetailHandler string1MethodCallByEEDetailHandler = new StringAppendCharMethodCallByEEDetailHandler(configureWrapper)) {
            Assert.assertTrue(string1MethodCallByEEDetailHandler.handleMethodCallByEECMKeyword(String.class.getName(), methodNameList));
            Assert.assertTrue(string1MethodCallByEEDetailHandler.handleMethodCallByEECMKeyword(StringUtils.class.getName(), methodNameList));
            Assert.assertTrue(string1MethodCallByEEDetailHandler.handleMethodCallByEECMKeyword(StringBuilder.class.getName(), Arrays.asList("append", "length")));
        }
    }

    @Test
    public void test1() {
        try (StringAllMethodCallByEEDetailHandler stringAllMethodCallByEEDetailHandler = new StringAllMethodCallByEEDetailHandler(configureWrapper)) {
            Assert.assertTrue(stringAllMethodCallByEEDetailHandler.handleMethodCallByEEC(String.class.getName()));
        }
    }
}
