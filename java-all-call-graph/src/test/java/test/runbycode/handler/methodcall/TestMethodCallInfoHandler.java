package test.runbycode.handler.methodcall;

import com.adrninistrator.jacg.dto.methodcall.parsed.AbstractMethodCallInfoParsed;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCallInfo;
import com.adrninistrator.jacg.handler.methodcall.MethodCallInfoHandler;
import com.adrninistrator.javacg2.common.enums.JavaCG2ConstantTypeEnum;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.junit.Assert;
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
public class TestMethodCallInfoHandler extends TestRunByCodeBase {
    private static final Logger logger = LoggerFactory.getLogger(TestMethodCallInfoHandler.class);

    @Test
    public void $test0WriteDb() {
        commonWriteDb();
    }

    @Test
    public void testQueryMethodCallInfoParsedObjArg() {
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

    @Test
    public void testQueryConstValueMCIByType() {
        try (MethodCallInfoHandler methodCallInfoHandler = new MethodCallInfoHandler(configureWrapper)) {
            doTestQueryConstValueMCIByType(methodCallInfoHandler, JavaCG2ConstantTypeEnum.CONSTTE_STRING, "");
            // MySQL中=方式查询空字符串与空格的效果相同，目前未处理
            doTestQueryConstValueMCIByType(methodCallInfoHandler, JavaCG2ConstantTypeEnum.CONSTTE_STRING, " ");
            doTestQueryConstValueMCIByType(methodCallInfoHandler, JavaCG2ConstantTypeEnum.CONSTTE_STRING, "1");
            doTestQueryConstValueMCIByType(methodCallInfoHandler, JavaCG2ConstantTypeEnum.CONSTTE_STRING, "ok");
            doTestQueryConstValueMCIByType(methodCallInfoHandler, JavaCG2ConstantTypeEnum.CONSTTE_INT, "1");
        }
    }

    private void doTestQueryConstValueMCIByType(MethodCallInfoHandler methodCallInfoHandler, JavaCG2ConstantTypeEnum javaCG2ConstantTypeEnum, String value) {
        List<WriteDbData4MethodCallInfo> list = methodCallInfoHandler.queryConstValueMCIByType(javaCG2ConstantTypeEnum, value);
        Assert.assertFalse(JavaCG2Util.isCollectionEmpty(list));
        printListContent(list, javaCG2ConstantTypeEnum.getType() + "@" + value);
    }
}