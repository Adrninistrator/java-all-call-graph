package com.adrninistrator.jacg.extensions.methodcall;

import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCall;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodInfo;
import com.adrninistrator.jacg.handler.method.MethodInfoHandler;
import com.adrninistrator.jacg.handler.methodcall.MethodCallInfoHandler;
import com.adrninistrator.javacg2.common.enums.JavaCG2CallTypeEnum;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2025/2/16
 * @description: java-all-call-graph 组件方法调用处理扩展类
 * 通过指定方法的调用参数添加方法调用，使用被调用对象，及被调用方法名
 */
public abstract class JACGObjInstanceMethodNameMCE extends AbstractJACGMethodCallExtension {

    private static final Logger logger = LoggerFactory.getLogger(JACGObjInstanceMethodNameMCE.class);
    private final MethodCallInfoHandler methodCallInfoHandler;
    private final MethodInfoHandler methodInfoHandler;
    private final String methodCallType;

    public JACGObjInstanceMethodNameMCE(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
        methodCallInfoHandler = new MethodCallInfoHandler(dbOperWrapper);
        methodInfoHandler = new MethodInfoHandler(dbOperWrapper);
        methodCallType = chooseMethodCallType();
    }

    @Override
    public boolean handle(WriteDbData4MethodCall methodCall) {
        if (!methodCallType.equals(methodCall.getCallType())) {
            // 返回未处理当前方法调用
            return false;
        }

        // 获取对应的原始方法调用ID
        int rawMethodCallId = Integer.parseInt(methodCall.getDescription());

        // 查询被调用对象类型列表
        List<String> calleeClassNameList = methodCallInfoHandler.queryMethodCallObjArgTypes(rawMethodCallId, chooseCalleeObjArgSeq());
        if (JavaCG2Util.isCollectionEmpty(calleeClassNameList)) {
            logger.warn("未查询到被调用对象类型 {}", rawMethodCallId);
        } else if (calleeClassNameList.size() > 1) {
            logger.warn("查询到被调用对象类型存在多种 {} {}", rawMethodCallId, StringUtils.join(calleeClassNameList, " "));
        }

        // 查询被调用方法名值列表
        List<String> calleeMethodNameList = methodCallInfoHandler.queryMethodCallObjArgValues(rawMethodCallId, chooseCalleeMethodNameArgSeq());
        if (JavaCG2Util.isCollectionEmpty(calleeMethodNameList)) {
            logger.warn("未查询到被调用方法名 {}", rawMethodCallId);
        } else if (calleeMethodNameList.size() > 1) {
            logger.warn("查询到被调用方法名存在多种 {} {}", rawMethodCallId, StringUtils.join(calleeMethodNameList, " "));
        }

        if (!JavaCG2Util.isCollectionEmpty(calleeClassNameList) && !JavaCG2Util.isCollectionEmpty(calleeMethodNameList)) {
            String calleeClassName = calleeClassNameList.get(0);
            String calleeMethodName = calleeMethodNameList.get(0);
            // 查询被调用方法
            List<WriteDbData4MethodInfo> methodInfoList = methodInfoHandler.queryMethodByClassMethodUpper(calleeClassName, calleeMethodName);
            if (JavaCG2Util.isCollectionEmpty(methodInfoList)) {
                logger.warn("未查询到被调用方法 {} {}", calleeClassName, calleeMethodName);
            } else {
                // 修改被调用方法
                WriteDbData4MethodInfo methodInfo = methodInfoList.get(0);
                String calleeSimpleClassName = dbOperWrapper.querySimpleClassName(calleeClassName);
                methodCall.setCalleeFullMethod(methodInfo.getFullMethod());
                methodCall.setCalleeSimpleClassName(calleeSimpleClassName);
                methodCall.setCalleeMethodHash(methodInfo.getMethodHash());
                methodCall.setCalleeMethodName(calleeMethodName);
                methodCall.setRawReturnType(methodInfo.getReturnType());
                methodCall.setCalleeJarNum(0);
            }
        }

        // 修改方法调用类型及描述
        methodCall.setCallType(JavaCG2CallTypeEnum.CTE_MANUAL_ADDED.getType());
        methodCall.setDescription("通过指定方法的调用参数添加方法调用，使用被调用对象，及被调用方法名 " + chooseMethodCallType() + " " + rawMethodCallId);
        return true;
    }

    // 选择被调用对象对应的参数序号，从1开始
    protected abstract int chooseCalleeObjArgSeq();

    // 选择被调用方法名对应的参数序号，从1开始
    protected abstract int chooseCalleeMethodNameArgSeq();

    /**
     * 选择需要替换的方法调用类型
     * 需要与 com.adrninistrator.jacg.extensions.methodcall.JavaCG2ObjInstanceMethodNameMCE#chooseMethodCallType() 返回配套
     *
     * @return
     */
    protected abstract String chooseMethodCallType();
}
