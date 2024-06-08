package test.runbycode.handler.methodcall;

import org.apache.commons.lang3.StringUtils;
import org.junit.Assert;
import org.junit.Test;
import test.runbycode.base.TestRunByCodeBase;
import test.runbycode.handler.methodcall.handler.RecordCallerMethodCallByEEDetailHandler;

import java.util.Arrays;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/7/3
 * @description:
 */
public class TestRecordCallerMethodCallByEEDetailHandler extends TestRunByCodeBase {

    @Test
    public void $test0WriteDb() {
        configureWrapper.setAllowAllClasses();
        commonWriteDb();
    }

    @Test
    public void test() {
        List<String> methodNameList = Arrays.asList("indexOf", "substring");
        try (RecordCallerMethodCallByEEDetailHandler recordCallerMethodCallByEEDetailHandler = new RecordCallerMethodCallByEEDetailHandler(configureWrapper)) {
            Assert.assertTrue(recordCallerMethodCallByEEDetailHandler.handleMethodCallByEECMKeyword(String.class.getName(), methodNameList));
            Assert.assertTrue(recordCallerMethodCallByEEDetailHandler.handleMethodCallByEECMKeyword(StringUtils.class.getName(), methodNameList));
            Assert.assertTrue(recordCallerMethodCallByEEDetailHandler.handleMethodCallByEECMKeyword(StringUtils.class.getName(), methodNameList, "test"));
            Assert.assertTrue(recordCallerMethodCallByEEDetailHandler.handleMethodCallByEECMKeyword(StringUtils.class.getName(), methodNameList, 1, "test"));

            printSetContent(recordCallerMethodCallByEEDetailHandler.getIndexOfCallerMethodSet(), "indexOf");
            printSetContent(recordCallerMethodCallByEEDetailHandler.getSubstringCallerMethodSet(), "substring");
        }
    }
}
