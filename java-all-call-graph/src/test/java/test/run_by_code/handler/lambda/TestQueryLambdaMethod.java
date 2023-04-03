package test.run_by_code.handler.lambda;

import com.adrninistrator.jacg.dto.lambda.LambdaMethodCallDetail;
import com.adrninistrator.jacg.handler.lambda.LambdaMethodHandlerByClassMethodName;
import com.adrninistrator.jacg.handler.lambda.LambdaMethodHandlerByClassNamePrefix;
import com.adrninistrator.jacg.handler.lambda.LambdaMethodHandlerByStreamMethod;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import test.run_by_code.handler.base.TestHandlerBase;

import java.util.List;
import java.util.Map;
import java.util.function.Function;

/**
 * @author adrninistrator
 * @date 2023/1/11
 * @description:
 */
public class TestQueryLambdaMethod extends TestHandlerBase {
    private static final Logger logger = LoggerFactory.getLogger(TestQueryLambdaMethod.class);

    @Test
    public void testQueryByClassNamePrefix() {
        try (LambdaMethodHandlerByClassNamePrefix lambdaMethodHandlerByClassNamePrefix = new LambdaMethodHandlerByClassNamePrefix(configureWrapper)) {
            queryByClassNamePrefix(lambdaMethodHandlerByClassNamePrefix, null, null);
            queryByClassNamePrefix(lambdaMethodHandlerByClassNamePrefix, "java.util.function.", null);
            queryByClassNamePrefix(lambdaMethodHandlerByClassNamePrefix, null, "java.util.stream.");
            queryByClassNamePrefix(lambdaMethodHandlerByClassNamePrefix, "java.util.function.", "java.util.stream.");
            queryByClassNamePrefix(lambdaMethodHandlerByClassNamePrefix, Function.class.getName(), Map.class.getName());
        }
    }

    @Test
    public void testQueryByClassMethodName() {
        try (LambdaMethodHandlerByClassMethodName lambdaMethodHandlerByClassMethodName = new LambdaMethodHandlerByClassMethodName(configureWrapper)) {
            queryByClassMethodName(lambdaMethodHandlerByClassMethodName, "java.util.function.Predicate", "test", true);
            queryByClassMethodName(lambdaMethodHandlerByClassMethodName, "java.util.stream.Stream", "map", false);
        }
    }

    @Test
    public void testQueryByStreamMethod() {
        try (LambdaMethodHandlerByStreamMethod lambdaMethodHandlerByStreamMethod = new LambdaMethodHandlerByStreamMethod(configureWrapper)) {
            queryByStreamMethod(lambdaMethodHandlerByStreamMethod, null, null);
            queryByStreamMethod(lambdaMethodHandlerByStreamMethod, true, true);
            queryByStreamMethod(lambdaMethodHandlerByStreamMethod, true, false);
            queryByStreamMethod(lambdaMethodHandlerByStreamMethod, false, null);
        }
    }

    private void queryByClassNamePrefix(LambdaMethodHandlerByClassNamePrefix lambdaMethodHandlerByClassNamePrefix,
                                        String lambdaCalleeClassNamePrefix,
                                        String lambdaNextCalleeClassNamePrefix) {
        List<LambdaMethodCallDetail> lambdaMethodCallDetailList = lambdaMethodHandlerByClassNamePrefix.queryByClassNamePrefixDetail(lambdaCalleeClassNamePrefix,
                lambdaNextCalleeClassNamePrefix);
        if (lambdaMethodCallDetailList == null) {
            return;
        }
        logger.info("{} {} {}", lambdaCalleeClassNamePrefix, lambdaNextCalleeClassNamePrefix, lambdaMethodCallDetailList.size());
        printListContent(lambdaMethodCallDetailList);
    }

    private void queryByClassMethodName(LambdaMethodHandlerByClassMethodName lambdaMethodHandlerByClassMethodName,
                                        String className,
                                        String methodName,
                                        boolean isLambdaCallee) {
        List<LambdaMethodCallDetail> lambdaMethodCallDetailList;
        if (isLambdaCallee) {
            lambdaMethodCallDetailList = lambdaMethodHandlerByClassMethodName.queryDetailByLambdaCallee(className, methodName);
        } else {
            lambdaMethodCallDetailList = lambdaMethodHandlerByClassMethodName.queryDetailByLambdaNextCallee(className, methodName);
        }
        if (lambdaMethodCallDetailList == null) {
            return;
        }
        logger.info("{} {} {}", className, methodName, lambdaMethodCallDetailList.size());
        printListContent(lambdaMethodCallDetailList);
    }

    private void queryByStreamMethod(LambdaMethodHandlerByStreamMethod lambdaMethodHandlerByStreamMethod, Boolean lambdaNextIsStream, Boolean lambdaNextIsIntermediate) {
        List<LambdaMethodCallDetail> lambdaMethodCallDetailList = lambdaMethodHandlerByStreamMethod.queryByStreamMethodDetail(lambdaNextIsStream,
                lambdaNextIsIntermediate);
        if (lambdaMethodCallDetailList == null) {
            return;
        }
        logger.info("{} {} {}", lambdaNextIsStream, lambdaNextIsIntermediate, lambdaMethodCallDetailList.size());
        printListContent(lambdaMethodCallDetailList);
    }
}
