package test.runbycode.handler.methodcall;

import org.junit.Test;
import test.runbycode.analysejavacg2jacg.TestAnalyseJavaCG2JACG1CheckLoggerClass;
import test.runbycode.base.TestRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2025/6/2
 * @description:
 */
public class TestAnalyseJACGTestCheckLoggerClass extends TestRunByCodeBase {

    @Test
    public void $test0WriteDb() {
        commonWriteDb();
    }

    @Test
    public void test() {
        TestAnalyseJavaCG2JACG1CheckLoggerClass testAnalyseJavaCG2JACG1CheckLoggerClass = new TestAnalyseJavaCG2JACG1CheckLoggerClass();
        testAnalyseJavaCG2JACG1CheckLoggerClass.check(configureWrapper);
    }
}
