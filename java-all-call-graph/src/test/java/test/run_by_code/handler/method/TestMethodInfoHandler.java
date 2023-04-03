package test.run_by_code.handler.method;

import com.adrninistrator.jacg.handler.method.MethodInfoHandler;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import test.call_graph.method_call.TestMCCallee;
import test.run_by_code.handler.base.TestHandlerBase;

/**
 * @author adrninistrator
 * @date 2023/3/26
 * @description:
 */
public class TestMethodInfoHandler extends TestHandlerBase {
    private static final Logger logger = LoggerFactory.getLogger(TestMethodInfoHandler.class);

    @Test
    public void testGetFullMethodByClassLine() {
        try (MethodInfoHandler methodInfoHandler = new MethodInfoHandler(configureWrapper)) {
            getFullMethodByClassLine(methodInfoHandler, TestMCCallee.class.getName(), 0);
            getFullMethodByClassLine(methodInfoHandler, TestMCCallee.class.getName(), 15);
            getFullMethodByClassLine(methodInfoHandler, TestMCCallee.class.getName(), 20);
        }
    }

    private void getFullMethodByClassLine(MethodInfoHandler methodInfoHandler, String className, int lineNumber) {
        String method = methodInfoHandler.getFullMethodByClassLine(className, lineNumber);
        logger.info("{} {} {}", className, lineNumber, method);
    }
}
