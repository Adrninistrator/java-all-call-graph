package com.adrninistrator.jacg.handler.methodcall;

import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCall;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCallInfo;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCallStaticFieldMCR;
import com.adrninistrator.jacg.dto.writedb.base.BaseWriteDbData4MethodCallClassField;
import com.adrninistrator.jacg.handler.base.BaseHandler;
import com.adrninistrator.jacg.handler.dto.methodcall.MethodCallWithInfo;
import com.adrninistrator.javacg2.common.enums.JavaCG2ConstantTypeEnum;
import com.adrninistrator.javacg2.util.JavaCG2ClassMethodUtil;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2025/5/27
 * @description: 查找指定的方法调用信息，并找到对应的方法调用
 */
public class FindMethodCallByCallInfoHandler extends BaseHandler {

    private final MethodCallClassFieldHandler methodCallClassFieldHandler;
    private final MethodCallInfoHandler methodCallInfoHandler;
    private final MethodCallHandler methodCallHandler;

    public FindMethodCallByCallInfoHandler(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
        methodCallClassFieldHandler = new MethodCallClassFieldHandler(dbOperWrapper);
        methodCallInfoHandler = new MethodCallInfoHandler(dbOperWrapper);
        methodCallHandler = new MethodCallHandler(dbOperWrapper);
    }

    public FindMethodCallByCallInfoHandler(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
        methodCallClassFieldHandler = new MethodCallClassFieldHandler(dbOperWrapper);
        methodCallInfoHandler = new MethodCallInfoHandler(dbOperWrapper);
        methodCallHandler = new MethodCallHandler(dbOperWrapper);
    }

    /**
     * 查询在方法调用中使用了指定常量值的信息，包含对应的方法调用
     *
     * @param javaCG2ConstantTypeEnum 常量类型
     * @param value                   常量值
     * @return
     */
    public List<MethodCallWithInfo<WriteDbData4MethodCallInfo>> queryMethodCallByConstantValue(JavaCG2ConstantTypeEnum javaCG2ConstantTypeEnum, String value) {
        return doQueryMethodCallByConstantValue(javaCG2ConstantTypeEnum, value, null, null, null);
    }

    /**
     * 查询指定方法被调用时使用了指定常量值的信息，包含对应的方法调用
     * 使用被调用类名与被调用方法名
     *
     * @param javaCG2ConstantTypeEnum 常量类型
     * @param value                   常量值
     * @param calleeClassName         被调用类名
     * @param calleeMethodName        被调用方法名，若为空则不限制
     * @return
     */
    public List<MethodCallWithInfo<WriteDbData4MethodCallInfo>> queryMethodCallByConstantValue4Method(JavaCG2ConstantTypeEnum javaCG2ConstantTypeEnum, String value,
                                                                                                      String calleeClassName, String calleeMethodName) {
        return doQueryMethodCallByConstantValue(javaCG2ConstantTypeEnum, value, calleeClassName, calleeMethodName, null);
    }

    /**
     * 查询指定方法被调用时使用了指定常量值的信息，包含对应的方法调用
     * 使用被调用完整方法
     *
     * @param javaCG2ConstantTypeEnum 常量类型
     * @param value                   常量值
     * @param calleeFullMethod        被调用完整方法
     * @return
     */
    public List<MethodCallWithInfo<WriteDbData4MethodCallInfo>> queryMethodCallByConstantValue4Method(JavaCG2ConstantTypeEnum javaCG2ConstantTypeEnum, String value,
                                                                                                      String calleeFullMethod) {
        return doQueryMethodCallByConstantValue(javaCG2ConstantTypeEnum, value, null, null, calleeFullMethod);
    }

    // 查询在方法调用中使用了指定常量值的信息，包含对应的方法调用
    private List<MethodCallWithInfo<WriteDbData4MethodCallInfo>> doQueryMethodCallByConstantValue(JavaCG2ConstantTypeEnum javaCG2ConstantTypeEnum, String value,
                                                                                                  String calleeClassName, String calleeMethodName, String calleeFullMethod) {
        List<MethodCallWithInfo<WriteDbData4MethodCallInfo>> methodCallWithInfoList = new ArrayList<>();
        List<WriteDbData4MethodCallInfo> methodCallInfoList = methodCallInfoHandler.queryConstValueMCIByType(javaCG2ConstantTypeEnum, value);
        if (JavaCG2Util.isCollectionEmpty(methodCallInfoList)) {
            return methodCallWithInfoList;
        }
        for (WriteDbData4MethodCallInfo methodCallInfo : methodCallInfoList) {
            WriteDbData4MethodCall methodCall = methodCallHandler.queryMethodCallByCallId(methodCallInfo.getCallId());
            // 检查是否需要跳过方法调用
            if (checkSkipMethodCall(methodCall, calleeClassName, calleeMethodName, calleeFullMethod)) {
                continue;
            }
            MethodCallWithInfo<WriteDbData4MethodCallInfo> methodCallWithInfo = new MethodCallWithInfo<>(methodCall, methodCallInfo);
            methodCallWithInfoList.add(methodCallWithInfo);
        }
        return methodCallWithInfoList;
    }

    // 检查是否需要跳过方法调用
    private boolean checkSkipMethodCall(WriteDbData4MethodCall methodCall, String calleeClassName, String calleeMethodName, String calleeFullMethod) {
        if (calleeClassName != null) {
            String calleeClassNameInMethodCall = JavaCG2ClassMethodUtil.getClassNameFromMethod(methodCall.getCalleeFullMethod());
            return !calleeClassName.equals(calleeClassNameInMethodCall) ||
                    (StringUtils.isNotBlank(calleeMethodName) && !calleeMethodName.equals(methodCall.getCalleeMethodName()));
        } else if (calleeFullMethod != null) {
            return !calleeFullMethod.equals(methodCall.getCalleeFullMethod());
        }
        return false;
    }

    /**
     * 查询在方法调用中使用了指定类的字段的信息，包含对应的方法调用。查询静态字段时，支持查询枚举常量使用情况
     *
     * @param queryStaticField true：查询静态字段 false：查询非静态字段
     * @param className        类名/枚举类名
     * @param fieldName        字段名/枚举常量名 可为空，若为空则查询指定类的全部字段；若非空则查询指定类的指定名称的字段
     * @return
     */
    public List<MethodCallWithInfo<BaseWriteDbData4MethodCallClassField>> queryMethodCallByClassField(boolean queryStaticField, String className, String fieldName) {
        return doQueryMethodCallByClassField(queryStaticField, className, fieldName, null, null, null);
    }

    /**
     * 查询指定方法被调用时使用了指定类的字段的信息，包含对应的方法调用。查询静态字段时，支持查询枚举常量使用情况
     * 使用被调用类名与被调用方法名
     *
     * @param queryStaticField true：查询静态字段 false：查询非静态字段
     * @param className        类名/枚举类名
     * @param fieldName        字段名/枚举常量名 可为空，若为空则查询指定类的全部字段；若非空则查询指定类的指定名称的字段
     * @param calleeClassName  被调用类名
     * @param calleeMethodName 被调用方法名，若为空则不限制
     * @return
     */
    public List<MethodCallWithInfo<BaseWriteDbData4MethodCallClassField>> queryMethodCallByClassField4Method(boolean queryStaticField, String className, String fieldName,
                                                                                                             String calleeClassName, String calleeMethodName) {
        return doQueryMethodCallByClassField(queryStaticField, className, fieldName, calleeClassName, calleeMethodName, null);
    }

    /**
     * 查询指定方法被调用时使用了指定类的字段的信息，包含对应的方法调用。查询静态字段时，支持查询枚举常量使用情况
     * 使用被调用完整方法
     *
     * @param queryStaticField true：查询静态字段 false：查询非静态字段
     * @param className        类名/枚举类名
     * @param fieldName        字段名/枚举常量名 可为空，若为空则查询指定类的全部字段；若非空则查询指定类的指定名称的字段
     * @param calleeFullMethod 被调用完整方法
     * @return
     */
    public List<MethodCallWithInfo<BaseWriteDbData4MethodCallClassField>> queryMethodCallByClassField4Method(boolean queryStaticField, String className, String fieldName,
                                                                                                             String calleeFullMethod) {
        return doQueryMethodCallByClassField(queryStaticField, className, fieldName, null, null, calleeFullMethod);
    }

    // 查询在方法调用中使用了指定类的字段的信息，包含对应的方法调用。查询静态字段时，支持查询枚举常量使用情况
    private List<MethodCallWithInfo<BaseWriteDbData4MethodCallClassField>> doQueryMethodCallByClassField(boolean queryStaticField, String className, String fieldName,
                                                                                                         String calleeClassName, String calleeMethodName,
                                                                                                         String calleeFullMethod) {
        List<MethodCallWithInfo<BaseWriteDbData4MethodCallClassField>> methodCallWithInfoList = new ArrayList<>();
        // 查询指定的类的字段在方法调用中的使用情况
        List<BaseWriteDbData4MethodCallClassField> methodCallClassFieldList;
        if (StringUtils.isBlank(fieldName)) {
            methodCallClassFieldList = methodCallClassFieldHandler.queryMethodCallClassField4Class(queryStaticField, className);
        } else {
            methodCallClassFieldList = methodCallClassFieldHandler.queryMethodCallClassField4ClassField(queryStaticField, className, fieldName);
        }
        if (JavaCG2Util.isCollectionEmpty(methodCallClassFieldList)) {
            return methodCallWithInfoList;
        }
        for (BaseWriteDbData4MethodCallClassField methodCallClassField : methodCallClassFieldList) {
            WriteDbData4MethodCall methodCall = methodCallHandler.queryMethodCallByCallId(methodCallClassField.getCallId());
            // 检查是否需要跳过方法调用
            if (checkSkipMethodCall(methodCall, calleeClassName, calleeMethodName, calleeFullMethod)) {
                continue;
            }
            MethodCallWithInfo<BaseWriteDbData4MethodCallClassField> methodCallWithInfo = new MethodCallWithInfo<>(methodCall, methodCallClassField);
            methodCallWithInfoList.add(methodCallWithInfo);
        }
        return methodCallWithInfoList;
    }

    /**
     * 查询在方法调用中使用了指定类的静态字段方法调用返回值，包含对应的方法调用（支持枚举常量值）
     *
     * @param className 类名/枚举类名
     * @param fieldName 字段名/枚举常量名
     * @return
     */
    public List<MethodCallWithInfo<WriteDbData4MethodCallStaticFieldMCR>> queryMethodCallByStaticFieldMCR(String className, String fieldName) {
        return doQueryMethodCallByStaticFieldMCR(className, fieldName, null, null, null);
    }

    /**
     * 查询指定方法被调用时使用了指定类的静态字段方法调用返回值，包含对应的方法调用（支持枚举常量值）
     * 使用被调用类名与被调用方法名
     *
     * @param className        类名/枚举类名
     * @param fieldName        字段名/枚举常量名
     * @param calleeClassName  被调用类名
     * @param calleeMethodName 被调用方法名，若为空则不限制
     * @return
     */
    public List<MethodCallWithInfo<WriteDbData4MethodCallStaticFieldMCR>> queryMethodCallByStaticFieldMCR4Method(String className, String fieldName, String calleeClassName,
                                                                                                                 String calleeMethodName) {
        return doQueryMethodCallByStaticFieldMCR(className, fieldName, calleeClassName, calleeMethodName, null);
    }

    /**
     * 查询指定方法被调用时使用了指定类的静态字段方法调用返回值，包含对应的方法调用（支持枚举常量值）
     * 使用被调用完整方法
     *
     * @param className        类名/枚举类名
     * @param fieldName        字段名/枚举常量名
     * @param calleeFullMethod 被调用完整方法
     * @return
     */
    public List<MethodCallWithInfo<WriteDbData4MethodCallStaticFieldMCR>> queryMethodCallByStaticFieldMCR4Method(String className, String fieldName, String calleeFullMethod) {
        return doQueryMethodCallByStaticFieldMCR(className, fieldName, null, null, calleeFullMethod);
    }

    // 查询指定方法被调用时使用了指定类的静态字段方法调用返回值，包含对应的方法调用（支持枚举常量值）
    private List<MethodCallWithInfo<WriteDbData4MethodCallStaticFieldMCR>> doQueryMethodCallByStaticFieldMCR(String className, String fieldName, String calleeClassName,
                                                                                                             String calleeMethodName, String calleeFullMethod) {
        List<MethodCallWithInfo<WriteDbData4MethodCallStaticFieldMCR>> methodCallWithInfoList = new ArrayList<>();
        List<WriteDbData4MethodCallStaticFieldMCR> methodCallStaticFieldMCRList;
        if (StringUtils.isBlank(fieldName)) {
            methodCallStaticFieldMCRList = methodCallClassFieldHandler.queryMethodCallStaticFieldMCR4Class(className);
        } else {
            methodCallStaticFieldMCRList = methodCallClassFieldHandler.queryMethodCallStaticFieldMCR4ClassField(className, fieldName);
        }
        if (JavaCG2Util.isCollectionEmpty(methodCallStaticFieldMCRList)) {
            return methodCallWithInfoList;
        }
        for (WriteDbData4MethodCallStaticFieldMCR methodCallStaticFieldMCR : methodCallStaticFieldMCRList) {
            WriteDbData4MethodCall methodCall = methodCallHandler.queryMethodCallByCallId(methodCallStaticFieldMCR.getCallId());
            // 检查是否需要跳过方法调用
            if (checkSkipMethodCall(methodCall, calleeClassName, calleeMethodName, calleeFullMethod)) {
                continue;
            }
            MethodCallWithInfo<WriteDbData4MethodCallStaticFieldMCR> methodCallWithInfo = new MethodCallWithInfo<>(methodCall, methodCallStaticFieldMCR);
            methodCallWithInfoList.add(methodCallWithInfo);
        }
        return methodCallWithInfoList;
    }
}
