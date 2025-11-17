package test.runbycode.analysejacg;

import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbInitializer;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCall;
import com.adrninistrator.jacg.handler.dto.string.StringAppendParseResult;
import com.adrninistrator.jacg.handler.dto.string.element.BaseStringElement;
import com.adrninistrator.jacg.handler.dto.string.element.StringElementConstant;
import com.adrninistrator.jacg.handler.methodcall.MethodCallHandler;
import com.adrninistrator.jacg.handler.methodcall.StringAppendHandler;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.apache.commons.lang3.StringUtils;
import org.junit.Assert;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2025/11/15
 * @description:
 */
public class TestAnalyseJACG3CheckSqlSupportPg {

    private static final Logger logger = LoggerFactory.getLogger(TestAnalyseJACG3CheckSqlSupportPg.class);

    @Test
    public void test() throws Exception {
        ConfigureWrapper configureWrapper = new ConfigureWrapper();
        DbOperWrapper dbOperWrapper = DbInitializer.genDbOperWrapper(configureWrapper, false, this);
        try (MethodCallHandler methodCallHandler = new MethodCallHandler(dbOperWrapper);
             StringAppendHandler stringAppendHandler = new StringAppendHandler(dbOperWrapper)) {
            List<WriteDbData4MethodCall> methodCallList = methodCallHandler.queryNormalMethodCallByCalleeClassMethod(DbOperWrapper.class.getName(), "cacheSql", true);
            if (JavaCG2Util.isCollectionEmpty(methodCallList)) {
                Assert.fail("未查询到对应的方法调用");
                return;
            }
            for (WriteDbData4MethodCall methodCall : methodCallList) {
                if (methodCall.getCallerFullMethod().startsWith(DbOperWrapper.class.getName())) {
                    continue;
                }
                List<StringAppendParseResult> stringAppendParseResultList = stringAppendHandler.parseStringAppend4MethodArg(methodCall.getCallId(), 2);
                if (JavaCG2Util.isCollectionEmpty(stringAppendParseResultList)) {
                    logger.error("未解析到字符串拼接信息，方法调用 {} {} {}", methodCall.getCallerFullMethod(), methodCall.getCallerLineNumber(), methodCall.getCalleeFullMethod());
                    Assert.fail("未解析到字符串拼接信息");
                }
                for (StringAppendParseResult stringAppendParseResult : stringAppendParseResultList) {
                    checkLimitXY(stringAppendParseResult, methodCall);

                    checkDeleteLimit(stringAppendParseResult, methodCall);
                }
            }

            logger.info("检查通过");
        }
    }

    private void checkLimitXY(StringAppendParseResult stringAppendParseResult, WriteDbData4MethodCall methodCall) {
        List<List<BaseStringElement>> stringElementListList = stringAppendParseResult.getStringElementListList();
        for (List<BaseStringElement> stringElementList : stringElementListList) {
            for (BaseStringElement stringElement : stringElementList) {
                if (!(stringElement instanceof StringElementConstant)) {
                    continue;
                }
                String constant = ((StringElementConstant) stringElement).getConstantValue();
                if (constant.contains(" limit ") && constant.contains(",")) {
                    logger.error("不能使用 limit x,y 方式，方法调用 {} {} {}", methodCall.getCallerFullMethod(), methodCall.getCallerLineNumber(), methodCall.getCalleeFullMethod());
                    Assert.fail("不能使用 limit x,y 方式");
                }
            }
        }
    }

    private void checkDeleteLimit(StringAppendParseResult stringAppendParseResult, WriteDbData4MethodCall methodCall) {
        List<List<BaseStringElement>> stringElementListList = stringAppendParseResult.getStringElementListList();
        List<BaseStringElement> firstStringElementList = stringElementListList.get(0);
        boolean deleteSql = false;
        for (BaseStringElement firstStringElement : firstStringElementList) {
            if (firstStringElement instanceof StringElementConstant) {
                String firstString = ((StringElementConstant) firstStringElement).getConstantValue();
                deleteSql = StringUtils.startsWithIgnoreCase(firstString.trim(), "delete");
            }
        }
        if (!deleteSql) {
            return;
        }
        List<BaseStringElement> lastStringElementList = stringElementListList.get(stringElementListList.size() - 1);
        for (BaseStringElement lastStringElement : lastStringElementList) {
            if (!(lastStringElement instanceof StringElementConstant)) {
                continue;
            }
            String lastString = ((StringElementConstant) lastStringElement).getConstantValue();
            if (StringUtils.endsWithIgnoreCase(lastString.trim(), "limit ?")) {
                logger.error("不能使用 delete limit ? 方式，方法调用 {} {} {}", methodCall.getCallerFullMethod(), methodCall.getCallerLineNumber(), methodCall.getCalleeFullMethod());
                Assert.fail("不能使用 delete limit ? 方式");
            }
        }
    }
}
