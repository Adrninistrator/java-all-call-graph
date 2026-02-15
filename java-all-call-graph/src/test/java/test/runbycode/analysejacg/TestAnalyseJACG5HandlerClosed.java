package test.runbycode.analysejacg;

import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCall;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCallInfo;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodInfo;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodReturnConstValue;
import com.adrninistrator.jacg.handler.base.BaseHandler;
import com.adrninistrator.jacg.handler.extendsimpl.JACGExtendsImplHandler;
import com.adrninistrator.jacg.handler.method.MethodArgReturnHandler;
import com.adrninistrator.jacg.handler.method.MethodInfoHandler;
import com.adrninistrator.jacg.handler.methodcall.MethodCallHandler;
import com.adrninistrator.jacg.handler.methodcall.MethodCallInfoHandler;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.javacg2.common.JavaCG2CommonNameConstants;
import com.adrninistrator.javacg2.common.enums.JavaCG2CalleeObjTypeEnum;
import com.adrninistrator.javacg2.common.enums.JavaCG2MethodCallInfoTypeEnum;
import com.adrninistrator.javacg2.util.JavaCG2ClassMethodUtil;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.apache.commons.lang3.StringUtils;
import org.junit.Assert;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import test.runbycode.base.TestRunByCodeBase;

import java.util.ArrayList;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2026/2/14
 * @description: 检查BaseHandler子类是否都有关闭（调用close方法）
 */
public class TestAnalyseJACG5HandlerClosed extends TestRunByCodeBase {

    private static final Logger logger = LoggerFactory.getLogger(TestAnalyseJACG5HandlerClosed.class);

    @Test
    public void test() {
        // 记录未调用close方法的方法调用信息列表
        List<String> notClosedMethodCallList = new ArrayList<>();
        try (JACGExtendsImplHandler jacgExtendsImplHandler = new JACGExtendsImplHandler(configureWrapper);
             MethodInfoHandler methodInfoHandler = new MethodInfoHandler(configureWrapper);
             MethodCallHandler methodCallHandler = new MethodCallHandler(configureWrapper);
             MethodCallInfoHandler methodCallInfoHandler = new MethodCallInfoHandler(configureWrapper);
             MethodArgReturnHandler methodArgReturnHandler = new MethodArgReturnHandler(configureWrapper)) {

            // 1. 获得所有BaseHandler子类（非抽象）
            List<String> handlerClassList = jacgExtendsImplHandler.queryChildClassListByFull(BaseHandler.class.getName(), false, true, false, true);
            logger.info("查询到BaseHandler非抽象子类数量: {}", handlerClassList.size());
            logger.info("BaseHandler非抽象子类列表: {}", StringUtils.join(handlerClassList, " "));

            for (String handlerClass : handlerClassList) {
                logger.info("开始处理类 {}", handlerClass);

                // 2. 获得BaseHandler子类指定的构造函数（参数数量为1，且类型为ConfigureWrapper）
                List<WriteDbData4MethodInfo> methodInfoList = methodInfoHandler.queryMethodInfoByClassMethod(handlerClass, JavaCG2CommonNameConstants.METHOD_NAME_INIT);
                if (JavaCG2Util.isCollectionEmpty(methodInfoList)) {
                    logger.info("类 {} 没有构造函数", handlerClass);
                    continue;
                }

                // 查找符合条件的构造函数
                WriteDbData4MethodInfo targetConstructor = null;
                for (WriteDbData4MethodInfo methodInfo : methodInfoList) {
                    List<String> argTypeList = JACGClassMethodUtil.genMethodArgTypeList(methodInfo.getFullMethod());
                    if (argTypeList.size() == 1 && ConfigureWrapper.class.getName().equals(argTypeList.get(0))) {
                        targetConstructor = methodInfo;
                        break;
                    }
                }

                if (targetConstructor == null) {
                    logger.info("类 {} 没有参数类型为ConfigureWrapper的构造函数", handlerClass);
                    continue;
                }

                logger.info("类 {} 找到指定构造函数: {}", handlerClass, targetConstructor.getFullMethod());

                // 3. 获得BaseHandler子类指定的构造函数被调用情况
                List<WriteDbData4MethodCall> constructorCallList = methodCallHandler.queryNormalMethodCallByCalleeFullMethod(targetConstructor.getFullMethod());
                if (JavaCG2Util.isCollectionEmpty(constructorCallList)) {
                    logger.info("类 {} 的指定构造函数没有被调用", handlerClass);
                    continue;
                }

                logger.info("类 {} 的指定构造函数被调用数量: {}", handlerClass, constructorCallList.size());

                // 4. 遍历构造函数被调用的方法调用ID，检查close方法是否被调用
                for (WriteDbData4MethodCall constructorCall : constructorCallList) {
                    // 判断是否为super调用
                    if (JavaCG2CalleeObjTypeEnum.COTE_THIS.getType().equals(constructorCall.getCalleeObjType())) {
                        // 被调用对象为this，属于对super方法的调用，不需要执行后续检查
                        logger.info("构造函数调用 {} 属于super调用，跳过检查", constructorCall.getCallId());
                        continue;
                    }

                    // 判断BaseHandler子类是否用于处理neo4j
                    if (checkUseNeo4j(methodInfoHandler, methodArgReturnHandler, handlerClass)) {
                        logger.info("Handler类 {} 用于处理neo4j，跳过检查", handlerClass);
                        continue;
                    }

                    checkHandlerClosed(jacgExtendsImplHandler, methodCallHandler, methodCallInfoHandler, handlerClass, constructorCall, notClosedMethodCallList);
                }
            }
        }

        // 最终判断
        if (notClosedMethodCallList.isEmpty()) {
            logger.info("检查通过，所有BaseHandler子类指定构造函数返回对象的close方法都有被调用");
            return;
        }

        logger.error("检查不通过，以下BaseHandler子类指定构造函数返回对象的close方法未被调用:\n{}", StringUtils.join(notClosedMethodCallList, "\n"));
        Assert.fail("检查不通过，存在BaseHandler子类指定构造函数返回对象的close方法未被调用");
    }

    /**
     * 判断BaseHandler子类是否用于处理neo4j
     *
     * @param methodInfoHandler      方法信息处理类
     * @param methodArgReturnHandler 方法参数返回处理类
     * @param handlerClass           BaseHandler子类名
     * @return true: 用于处理neo4j，不需要执行后续检查 false: 不是用于处理neo4j，需要执行后续检查
     */
    private boolean checkUseNeo4j(MethodInfoHandler methodInfoHandler,
                                  MethodArgReturnHandler methodArgReturnHandler,
                                  String handlerClass) {
        // 查询Handler类是否有useNeo4j方法
        List<WriteDbData4MethodInfo> useNeo4jMethodList = methodInfoHandler.queryMethodInfoByClassMethod(handlerClass, "useNeo4j");
        if (JavaCG2Util.isCollectionEmpty(useNeo4jMethodList)) {
            // 当前类没有useNeo4j方法，使用父类的默认实现（返回false），需要执行后续检查
            return false;
        }

        // 获取无参的useNeo4j()方法
        WriteDbData4MethodInfo useNeo4jMethod = null;
        for (WriteDbData4MethodInfo methodInfo : useNeo4jMethodList) {
            List<String> argTypeList = JACGClassMethodUtil.genMethodArgTypeList(methodInfo.getFullMethod());
            if (argTypeList.isEmpty()) {
                useNeo4jMethod = methodInfo;
                break;
            }
        }

        if (useNeo4jMethod == null) {
            return false;
        }

        // 使用MethodArgReturnHandler.queryMethodReturnConstValue查询方法返回的常量值
        List<WriteDbData4MethodReturnConstValue> returnConstValueList = methodArgReturnHandler.queryMethodReturnConstValue(useNeo4jMethod.getMethodHash());
        if (JavaCG2Util.isCollectionEmpty(returnConstValueList)) {
            return false;
        }

        // 检查返回值是否为true（boolean类型的true在数据库中存储为1）
        for (WriteDbData4MethodReturnConstValue returnConstValue : returnConstValueList) {
            String constValue = returnConstValue.getConstValue();
            if ("1".equals(constValue) || "true".equalsIgnoreCase(constValue)) {
                return true;
            }
        }

        return false;
    }

    /**
     * 检查BaseHandler子类指定构造函数返回对象的close方法是否被调用
     *
     * @param jacgExtendsImplHandler 继承实现处理类
     * @param methodCallHandler      方法调用处理类
     * @param methodCallInfoHandler  方法调用信息处理类
     * @param handlerClass           BaseHandler子类名
     * @param constructorCall        构造函数被调用的方法调用信息
     * @param notClosedMethodCallList 未调用close方法的方法调用信息列表
     */
    private void checkHandlerClosed(JACGExtendsImplHandler jacgExtendsImplHandler,
                                    MethodCallHandler methodCallHandler,
                                    MethodCallInfoHandler methodCallInfoHandler,
                                    String handlerClass,
                                    WriteDbData4MethodCall constructorCall,
                                    List<String> notClosedMethodCallList) {
        // 获得以BaseHandler子类指定的构造函数返回值作为被调用对象的方法调用
        String callerMethodHash = constructorCall.getCallerMethodHash();
        String constructorCallId = String.valueOf(constructorCall.getCallId());

        List<WriteDbData4MethodCallInfo> methodCallInfoList = methodCallInfoHandler.queryMethodCallInfo4CallerByTypeValue(
                callerMethodHash,
                constructorCallId,
                JavaCG2MethodCallInfoTypeEnum.MCIT_METHOD_CALL_RETURN_CALL_ID.getType()
        );

        if (JavaCG2Util.isCollectionEmpty(methodCallInfoList)) {
            // 返回为空，说明当前BaseHandler子类指定的构造函数返回对象不满足close()方法有被调用
            String info = String.format("Handler类: %s, 调用方方法: %s, 行号: %d, 构造函数: %s",
                    handlerClass,
                    constructorCall.getCallerFullMethod(),
                    constructorCall.getCallerLineNumber(),
                    constructorCall.getCalleeFullMethod());
            notClosedMethodCallList.add(info);
            logger.warn("构造函数调用 {} 没有以返回值作为被调用对象的方法调用，Handler类: {}, 调用方方法: {}",
                    constructorCall.getCallId(), handlerClass, constructorCall.getCallerFullMethod());
            return;
        }

        // 检查是否有close方法被调用
        boolean closedFlag = false;
        for (WriteDbData4MethodCallInfo methodCallInfo : methodCallInfoList) {
            // objArgsSeq为0表示被调用对象
            if (methodCallInfo.getObjArgsSeq() != 0) {
                continue;
            }

            // 获取对应的方法调用信息
            int callId = methodCallInfo.getCallId();
            WriteDbData4MethodCall methodCall = methodCallHandler.queryMethodCallByCallId(callId);
            if (methodCall == null) {
                continue;
            }

            // 检查被调用方法是否为close()
            if ("close".equals(methodCall.getCalleeMethodName())) {
                // 检查方法是否无参数
                List<String> calleeArgTypeList = JACGClassMethodUtil.genMethodArgTypeList(methodCall.getCalleeFullMethod());
                if (!calleeArgTypeList.isEmpty()) {
                    // close方法有参数，不是无参的close()方法，继续检查下一个
                    continue;
                }

                // 检查被调用类是否为当前Handler类或其父类
                String calleeClassName = JavaCG2ClassMethodUtil.getClassNameFromMethod(methodCall.getCalleeFullMethod());
                if (jacgExtendsImplHandler.checkExtendsOrImplFull(handlerClass, calleeClassName) ||
                        jacgExtendsImplHandler.checkExtendsOrImplFull(calleeClassName, handlerClass) ||
                        handlerClass.equals(calleeClassName)) {
                    logger.info("构造函数调用 {} 返回对象的close()方法已被调用，call_id: {}", constructorCall.getCallId(), callId);
                    closedFlag = true;
                    break;
                }
            }
        }

        if (!closedFlag) {
            // 记录未调用close方法的方法调用信息
            String info = String.format("Handler类: %s, 调用方方法: %s, 行号: %d, 构造函数: %s",
                    handlerClass,
                    constructorCall.getCallerFullMethod(),
                    constructorCall.getCallerLineNumber(),
                    constructorCall.getCalleeFullMethod());
            notClosedMethodCallList.add(info);
            logger.warn("构造函数调用 {} 返回对象的close方法未被调用，Handler类: {}, 调用方方法: {}",
                    constructorCall.getCallId(), handlerClass, constructorCall.getCallerFullMethod());
        }
    }
}
