package test.runbycode.handler.method;

import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCatch;
import com.adrninistrator.jacg.handler.exception.MethodExceptionHandler;
import com.adrninistrator.javacg.util.JavaCGUtil;
import org.junit.Assert;
import org.junit.Test;
import test.runbycode.base.TestRunByCodeBase;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2024/1/2
 * @description:
 */
public class TestMethodExceptionHandler extends TestRunByCodeBase {

    @Test
    public void $test0WriteDb() {
        commonWriteDb();
    }

    @Test
    public void testQueryMethodCatchByType() {
        try (MethodExceptionHandler methodExceptionBaseHandler = new MethodExceptionHandler(configureWrapper)) {
            List<WriteDbData4MethodCatch> methodTryCatchFinallyList = methodExceptionBaseHandler.queryMethodCatchBySimpleCatchExceptionType(Exception.class.getName());
            Assert.assertFalse(JavaCGUtil.isCollectionEmpty(methodTryCatchFinallyList));
            printListContent(methodTryCatchFinallyList);
        }
    }
}
