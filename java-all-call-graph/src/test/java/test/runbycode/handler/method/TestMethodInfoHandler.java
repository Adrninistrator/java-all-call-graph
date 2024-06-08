package test.runbycode.handler.method;

import com.adrninistrator.jacg.handler.method.MethodInfoHandler;
import org.apache.commons.lang3.StringUtils;
import org.junit.Assert;
import org.junit.Test;
import test.callgraph.methodcall.TestMCCallee;
import test.runbycode.base.TestRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2023/3/26
 * @description:
 */
public class TestMethodInfoHandler extends TestRunByCodeBase {

    @Test
    public void $test0WriteDb() {
        commonWriteDb();
    }

    @Test
    public void testGetFullMethodByClassLine() {
        try (MethodInfoHandler methodInfoHandler = new MethodInfoHandler(configureWrapper)) {
            getFullMethodByClassLine(methodInfoHandler, TestMCCallee.class.getName(), 0, false);
            getFullMethodByClassLine(methodInfoHandler, TestMCCallee.class.getName(), 15, false);
            getFullMethodByClassLine(methodInfoHandler, TestMCCallee.class.getName(), 20, true);
        }
    }

    private void getFullMethodByClassLine(MethodInfoHandler methodInfoHandler, String className, int lineNumber, boolean exists) {
        String method = methodInfoHandler.queryFullMethodByClassLine(className, lineNumber);
        Assert.assertEquals(exists, StringUtils.isNotBlank(method));
        printObjectContent(method, className, String.valueOf(lineNumber), method);
    }
}
