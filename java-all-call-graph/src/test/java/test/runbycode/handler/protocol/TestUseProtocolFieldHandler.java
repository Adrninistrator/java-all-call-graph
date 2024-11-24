package test.runbycode.handler.protocol;

import org.junit.Test;
import test.runbycode.base.TestRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2024/10/27
 * @description:
 */
public class TestUseProtocolFieldHandler extends TestRunByCodeBase {

    @Test
    public void $test0WriteDb() {
        commonWriteDbForce();
    }

    @Test
    public void test() {
        try (TestProtocolFieldHandler testProtocolFieldHandler = new TestProtocolFieldHandler(configureWrapper)) {
            testProtocolFieldHandler.handle();
        }
    }
}
