package test.runbycode.handler.methodcall;

import com.adrninistrator.jacg.dto.methodcall.parsed.AbstractMethodCallInfoParsed;
import com.adrninistrator.jacg.handler.methodcall.MethodCallInfoHandler;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import test.runbycode.base.TestRunByCodeBase;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/11/9
 * @description:
 */
public class TestMethodCallInfo extends TestRunByCodeBase {
    private static final Logger logger = LoggerFactory.getLogger(TestMethodCallInfo.class);

    @Test
    public void $test0WriteDb() {
        commonWriteDb();
    }

    @Test
    public void test() {
        try (MethodCallInfoHandler methodCallInfoHandler = new MethodCallInfoHandler(configureWrapper)) {
            for (int i = 0; i < 1000; i++) {
                for (int j = 0; j < 3; j++) {
                    List<AbstractMethodCallInfoParsed> methodCallInfoParsedList = methodCallInfoHandler.queryMethodCallInfoParsedObjArg(i, j, false);
                    if (!JavaCG2Util.isCollectionEmpty(methodCallInfoParsedList)) {
                        logger.info("{}", methodCallInfoParsedList.size());
                    }
                }
            }
        }
    }
}
