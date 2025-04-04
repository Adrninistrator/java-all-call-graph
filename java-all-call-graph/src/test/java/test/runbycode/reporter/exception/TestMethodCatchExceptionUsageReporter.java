package test.runbycode.reporter.exception;

import com.adrninistrator.jacg.handler.exception.reporter.MethodCatchExceptionUsageReporter;
import com.adrninistrator.javacg2.dto.counter.JavaCG2Counter;
import org.junit.Assert;
import org.junit.Test;
import test.runbycode.base.TestRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2024/1/3
 * @description:
 */
public class TestMethodCatchExceptionUsageReporter extends TestRunByCodeBase {

    public static final String[] LOGGER_METHOD_ARRAY = new String[]{
            "org.slf4j.Logger:error",
            "org.slf4j.Logger:info",
            "org.slf4j.Logger:warn"
    };

    @Test
    public void test1() {
        MethodCatchExceptionUsageReporter methodCatchExceptionUsageReporter = new MethodCatchExceptionUsageReporter(javaCG2ConfigureWrapper, configureWrapper, "build" +
                "/catch_exception_report", false, false);
        Assert.assertTrue(methodCatchExceptionUsageReporter.genMethodCatchExceptionUsageReporter(LOGGER_METHOD_ARRAY));
    }

    @Test
    public void testSkipWriteDb() {
        JavaCG2Counter catchSeq = new JavaCG2Counter(10000);
        MethodCatchExceptionUsageReporter methodCatchExceptionUsageReporter = new MethodCatchExceptionUsageReporter(javaCG2ConfigureWrapper, configureWrapper, "build" +
                "/catch_exception_report", false, true);
        methodCatchExceptionUsageReporter.setCatchSeq(catchSeq);
        Assert.assertTrue(methodCatchExceptionUsageReporter.genMethodCatchExceptionUsageReporter(LOGGER_METHOD_ARRAY));
    }
}
