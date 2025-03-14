package test.runbycode.handler.method;

import com.adrninistrator.jacg.dboper.DbInitializer;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.method.MethodArgAndCommonFieldInfo;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodArgGenericsType;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodArgument;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodInfo;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodReturnGenericsType;
import com.adrninistrator.jacg.handler.method.MethodArgReturnHandler;
import com.adrninistrator.jacg.handler.method.MethodInfoHandler;
import com.adrninistrator.javacg2.common.enums.JavaCG2YesNoEnum;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.junit.Assert;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import test.callgraph.field.cycle.TestFieldCycle1;
import test.callgraph.field.cycle.TestFieldGenericsCycle1;
import test.callgraph.methodargument.TestArgumentGenerics1;
import test.callgraph.methodreturn.TestMethodReturnGenericsType1;
import test.runbycode.base.TestRunByCodeBase;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2024/10/26
 * @description:
 */
public class TestMethodArgReturnHandler extends TestRunByCodeBase {
    private static final Logger logger = LoggerFactory.getLogger(TestMethodArgReturnHandler.class);

    @Test
    public void $test0WriteDb() {
        commonWriteDb();
    }

    @Test
    public void testQueryMethodArgAndGenericsType() {
        try (MethodArgReturnHandler methodArgReturnHandler = new MethodArgReturnHandler(configureWrapper)) {
            doQueryMethodArgAndGenericsType(methodArgReturnHandler, TestArgumentGenerics1.class.getName());
        }
    }

    @Test
    public void testQueryMethodReturnAndGenericsType() {
        try (MethodInfoHandler methodInfoHandler = new MethodInfoHandler(configureWrapper);
             MethodArgReturnHandler methodArgReturnHandler = new MethodArgReturnHandler(configureWrapper)) {
            doQueryMethodReturnAndGenericsType(methodInfoHandler, methodArgReturnHandler, TestMethodReturnGenericsType1.class.getName());
        }
    }

    private void doQueryMethodArgAndGenericsType(MethodArgReturnHandler methodArgReturnHandler, String className) {
        DbOperWrapper dbOperWrapper = DbInitializer.genDbOperWrapper(configureWrapper, this);
        List<String> fullMethodList = dbOperWrapper.queryMethodByClassName(className);
        Assert.assertFalse(JavaCG2Util.isCollectionEmpty(fullMethodList));
        for (String fullMethod : fullMethodList) {
            List<WriteDbData4MethodArgument> methodArgumentList = methodArgReturnHandler.queryMethodArgumentByMethod(fullMethod);
            if (JavaCG2Util.isCollectionEmpty(methodArgumentList)) {
                logger.info("当前方法未查询到参数 {}", fullMethod);
                continue;
            }
            for (WriteDbData4MethodArgument methodArgument : methodArgumentList) {
                printObjectContent(methodArgument, "方法参数 " + fullMethod + " " + methodArgument.getArgSeq());
                if (JavaCG2YesNoEnum.isYes(methodArgument.getExistsGenericsType())) {
                    List<WriteDbData4MethodArgGenericsType> methodArgGenericsTypeList = methodArgReturnHandler.queryMethodArgGenericsTypeByMethodArg(fullMethod,
                            methodArgument.getArgSeq());
                    printListContent(methodArgGenericsTypeList, "方法参数的泛型类型 " + fullMethod + " " + methodArgument.getArgSeq());
                    List<String> typeList = methodArgReturnHandler.queryGenericsTypeInMethodArg(fullMethod, methodArgument.getArgSeq(), false);
                    printListContent(typeList, "方法参数的泛型类型中的类型 " + fullMethod + " " + methodArgument.getArgSeq());
                    List<String> customTypeList = methodArgReturnHandler.queryGenericsTypeInMethodArg(fullMethod, methodArgument.getArgSeq(), true);
                    printListContent(customTypeList, "方法参数的泛型类型中的自定义类型 " + fullMethod + " " + methodArgument.getArgSeq());
                } else {
                    logger.info("当前方法不存在泛型类型 {} {}", fullMethod, methodArgument.getArgSeq());
                }
            }
        }
    }

    private void doQueryMethodReturnAndGenericsType(MethodInfoHandler methodInfoHandler, MethodArgReturnHandler methodArgReturnHandler, String className) {
        List<WriteDbData4MethodInfo> methodInfoList = methodInfoHandler.queryMethodInfoByClass(className);
        Assert.assertFalse(JavaCG2Util.isCollectionEmpty(methodInfoList));
        for (WriteDbData4MethodInfo methodInfo : methodInfoList) {
            String fullMethod = methodInfo.getFullMethod();
            printObjectContent(methodInfo, "方法信息 " + fullMethod);
            if (JavaCG2YesNoEnum.isYes(methodInfo.getReturnExistsGenericsType())) {
                List<WriteDbData4MethodReturnGenericsType> methodArgGenericsTypeList = methodArgReturnHandler.queryReturnGenericsTypeByMethod(fullMethod);
                printListContent(methodArgGenericsTypeList, "方法返回类型的泛型类型 " + fullMethod);
                List<String> typeList = methodArgReturnHandler.queryGenericsTypeInMethodReturn(fullMethod, false);
                printListContent(typeList, "方法返回类型的泛型类型中的类型 " + fullMethod);
                List<String> customTypeList = methodArgReturnHandler.queryGenericsTypeInMethodReturn(fullMethod, true);
                printListContent(customTypeList, "方法返回类型的泛型类型中的自定义类型 " + fullMethod);
            } else {
                logger.info("当前方法返回类型不存在泛型类型 {}", fullMethod);
            }
        }
    }

    @Test
    public void queryMethodArgAndCommonFieldInfo() {
        try (MethodArgReturnHandler methodArgReturnHandler = new MethodArgReturnHandler(configureWrapper)) {
            doQueryMethodArgAndCommonFieldInfo(methodArgReturnHandler, TestFieldCycle1.class);
            doQueryMethodArgAndCommonFieldInfo(methodArgReturnHandler, TestFieldGenericsCycle1.class);
        }
    }

    private void doQueryMethodArgAndCommonFieldInfo(MethodArgReturnHandler methodArgReturnHandler, Class<?> clazz) {
        DbOperWrapper dbOperWrapper = DbInitializer.genDbOperWrapper(configureWrapper, this);
        List<String> fullMethodList = dbOperWrapper.queryMethodByClassName(clazz.getName());
        for (String fullMethod : fullMethodList) {
            List<MethodArgAndCommonFieldInfo> methodArgAndCommonFieldInfoList = methodArgReturnHandler.queryMethodArgAndCommonFieldInfo(fullMethod);
            printListContent(methodArgAndCommonFieldInfoList, fullMethod);
        }
    }
}
