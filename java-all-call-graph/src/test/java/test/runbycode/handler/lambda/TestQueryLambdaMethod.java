package test.runbycode.handler.lambda;

import com.adrninistrator.jacg.dto.lambda.LambdaMethodCall;
import com.adrninistrator.jacg.handler.lambda.LambdaMethodHandlerByClassMethodName;
import com.adrninistrator.jacg.handler.lambda.LambdaMethodHandlerByClassNamePrefix;
import com.adrninistrator.jacg.handler.lambda.LambdaMethodHandlerByStreamMethod;
import org.junit.Test;
import test.runbycode.base.TestRunByCodeBase;

import java.util.List;
import java.util.Map;
import java.util.function.Function;

/**
 * @author adrninistrator
 * @date 2023/1/11
 * @description:
 */
public class TestQueryLambdaMethod extends TestRunByCodeBase {

    @Test
    public void $test0WriteDb() {
        commonWriteDb();
    }

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
                                        String lambdaNextClassNamePrefix) {
        List<LambdaMethodCall> lambdaMethodCallDetailList = lambdaMethodHandlerByClassNamePrefix.queryByClassNamePrefix(lambdaCalleeClassNamePrefix,
                lambdaNextClassNamePrefix);
        if (lambdaMethodCallDetailList == null) {
            return;
        }
        printListContent(lambdaMethodCallDetailList, lambdaCalleeClassNamePrefix, lambdaNextClassNamePrefix, String.valueOf(lambdaMethodCallDetailList.size()));
    }

    private void queryByClassMethodName(LambdaMethodHandlerByClassMethodName lambdaMethodHandlerByClassMethodName,
                                        String className,
                                        String methodName,
                                        boolean isLambdaCallee) {
        List<LambdaMethodCall> lambdaMethodCallList;
        if (isLambdaCallee) {
            lambdaMethodCallList = lambdaMethodHandlerByClassMethodName.queryDetailByLambdaCallee(className, methodName);
        } else {
            lambdaMethodCallList = lambdaMethodHandlerByClassMethodName.queryDetailByLambdaNext(className, methodName);
        }
        if (lambdaMethodCallList == null) {
            return;
        }
        printListContent(lambdaMethodCallList, className, methodName, String.valueOf(lambdaMethodCallList.size()));
    }

    private void queryByStreamMethod(LambdaMethodHandlerByStreamMethod lambdaMethodHandlerByStreamMethod, Boolean lambdaNextIsStream, Boolean lambdaNextIsIntermediate) {
        List<LambdaMethodCall> lambdaMethodCallList = lambdaMethodHandlerByStreamMethod.queryByStreamMethod(lambdaNextIsStream, lambdaNextIsIntermediate);
        if (lambdaMethodCallList == null) {
            return;
        }
        printListContent(lambdaMethodCallList, String.valueOf(lambdaNextIsStream), String.valueOf(lambdaNextIsIntermediate),
                String.valueOf(lambdaMethodCallList.size()));
    }
}
