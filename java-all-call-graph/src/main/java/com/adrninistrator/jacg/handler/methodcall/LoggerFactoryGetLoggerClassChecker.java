package com.adrninistrator.jacg.handler.methodcall;

import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCall;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCallInfo;
import com.adrninistrator.jacg.handler.base.BaseHandler;
import com.adrninistrator.javacg2.common.JavaCG2CommonNameConstants;
import com.adrninistrator.javacg2.common.enums.JavaCG2MethodCallInfoTypeEnum;
import com.adrninistrator.javacg2.util.JavaCG2ClassMethodUtil;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2025/6/1
 * @description: 检查LoggerFactory.getLogger方法参数是否都是当前类
 */
public class LoggerFactoryGetLoggerClassChecker extends BaseHandler {

    private static final Logger logger = LoggerFactory.getLogger(LoggerFactoryGetLoggerClassChecker.class);
    private final MethodCallHandler methodCallHandler;
    private final MethodCallInfoHandler methodCallInfoHandler;

    public LoggerFactoryGetLoggerClassChecker(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
        methodCallHandler = new MethodCallHandler(dbOperWrapper);
        methodCallInfoHandler = new MethodCallInfoHandler(dbOperWrapper);
    }

    public LoggerFactoryGetLoggerClassChecker(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
        methodCallHandler = new MethodCallHandler(dbOperWrapper);
        methodCallInfoHandler = new MethodCallInfoHandler(dbOperWrapper);
    }

    /**
     * 查询LoggerFactory.getLogger方法参数不是当前类的类列表
     *
     * @return
     */
    public Set<String> queryInvalidClassList() {
        Set<String> invalidClassSet = new HashSet<>();
        // 查询org.slf4j.LoggerFactory:getLogger(java.lang.Class)方法被调用情况
        String loggerFactoryGetLoggerMethod = JavaCG2ClassMethodUtil.formatFullMethod(LoggerFactory.class.getName(), "getLogger", Class.class);
        List<WriteDbData4MethodCall> methodCallList = methodCallHandler.queryNormalMethodCallByCalleeMethodReturnType(loggerFactoryGetLoggerMethod, Logger.class.getName());
        if (JavaCG2Util.isCollectionEmpty(methodCallList)) {
            logger.warn("未查找到相关方法调用");
            return invalidClassSet;
        }
        for (WriteDbData4MethodCall methodCall : methodCallList) {
            if (!JavaCG2CommonNameConstants.METHOD_NAME_CLINIT.equals(methodCall.getCallerMethodName())) {
                // 跳过非<clinit>方法
                continue;
            }
            // 查询对应方法调用参数1可能的类型
            List<WriteDbData4MethodCallInfo> methodCallInfoList = methodCallInfoHandler.queryMethodCallInfoObjArg(methodCall.getCallId(), 1);
            if (JavaCG2Util.isCollectionEmpty(methodCallInfoList)) {
                continue;
            }
            List<String> getLoggerArg1TypeList = new ArrayList<>();
            for (WriteDbData4MethodCallInfo methodCallInfo : methodCallInfoList) {
                if (JavaCG2MethodCallInfoTypeEnum.MCIT_TYPE.getType().equals(methodCallInfo.getType()) && !getLoggerArg1TypeList.contains(methodCallInfo.getTheValue())) {
                    getLoggerArg1TypeList.add(methodCallInfo.getTheValue());
                }
            }
            String callerClassName = JavaCG2ClassMethodUtil.getClassNameFromMethod(methodCall.getCallerFullMethod());
            if (getLoggerArg1TypeList.size() > 1 ||
                    (getLoggerArg1TypeList.size() == 1 && !callerClassName.equals(getLoggerArg1TypeList.get(0)))) {
                invalidClassSet.add(callerClassName);
            }
        }
        return invalidClassSet;
    }
}
