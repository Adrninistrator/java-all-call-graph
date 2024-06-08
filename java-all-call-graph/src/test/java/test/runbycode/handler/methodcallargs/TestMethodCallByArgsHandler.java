package test.runbycode.handler.methodcallargs;

import org.junit.Assert;
import org.junit.Test;
import test.runbycode.base.TestRunByCodeBase;
import test.runbycode.handler.methodcallargs.handler.CharsetMethodCallByArgsHandler;
import test.runbycode.handler.methodcallargs.handler.TypeMethodCallByArgsHandler;
import test.runbycode.handler.methodcallargs.handler.ValueMethodCallByArgsHandler;

/**
 * @author adrninistrator
 * @date 2023/6/28
 * @description:
 */
public class TestMethodCallByArgsHandler extends TestRunByCodeBase {

    @Test
    public void $test0WriteDb() {
        commonWriteDb();
    }

    @Test
    public void test2() {
        try (CharsetMethodCallByArgsHandler charsetMethodCallArgsHandler = new CharsetMethodCallByArgsHandler(configureWrapper)) {
            Assert.assertTrue(charsetMethodCallArgsHandler.handleMethodCallByArgs());
        }
    }

    @Test
    public void test3() {
        try (TypeMethodCallByArgsHandler typeMethodCallArgsHandler = new TypeMethodCallByArgsHandler(configureWrapper)) {
            Assert.assertTrue(typeMethodCallArgsHandler.handleMethodCallByArgs());
        }
    }

    @Test
    public void test4() {
        try (ValueMethodCallByArgsHandler valueMethodCallByArgsHandler = new ValueMethodCallByArgsHandler(configureWrapper)) {
            Assert.assertTrue(valueMethodCallByArgsHandler.handleMethodCallByArgs());
        }
    }
}
