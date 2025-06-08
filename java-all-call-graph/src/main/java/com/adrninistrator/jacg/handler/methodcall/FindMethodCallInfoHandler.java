package com.adrninistrator.jacg.handler.methodcall;

import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCall;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCallInfo;
import com.adrninistrator.jacg.handler.base.BaseHandler;
import com.adrninistrator.jacg.handler.classes.ClassInfoHandler;
import com.adrninistrator.jacg.handler.dto.methodcall.MethodCallObjArgValue;
import com.adrninistrator.jacg.handler.dto.methodcall.MethodCallWithValueSupportEnum;
import com.adrninistrator.jacg.handler.enums.EnumsHandler;
import com.adrninistrator.javacg2.common.enums.JavaCG2ConstantTypeEnum;
import com.adrninistrator.javacg2.common.enums.JavaCG2MethodCallInfoTypeEnum;
import com.adrninistrator.javacg2.dto.field.ClassFieldMethodCall;
import com.adrninistrator.javacg2.util.JavaCG2ByteCodeUtil;
import com.adrninistrator.javacg2.util.JavaCG2ClassMethodUtil;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2025/5/30
 * @description: 查找指定方法被调用时使用的参数信息处理类
 */
public class FindMethodCallInfoHandler extends BaseHandler {
    private static final Logger logger = LoggerFactory.getLogger(FindMethodCallInfoHandler.class);

    private final ClassInfoHandler classInfoHandler;
    private final EnumsHandler enumsHandler;
    private final MethodCallHandler methodCallHandler;
    private final MethodCallInfoHandler methodCallInfoHandler;

    public FindMethodCallInfoHandler(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
        classInfoHandler = new ClassInfoHandler(dbOperWrapper);
        enumsHandler = new EnumsHandler(dbOperWrapper);
        methodCallHandler = new MethodCallHandler(dbOperWrapper);
        methodCallInfoHandler = new MethodCallInfoHandler(dbOperWrapper);
    }

    public FindMethodCallInfoHandler(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
        classInfoHandler = new ClassInfoHandler(dbOperWrapper);
        enumsHandler = new EnumsHandler(dbOperWrapper);
        methodCallHandler = new MethodCallHandler(dbOperWrapper);
        methodCallInfoHandler = new MethodCallInfoHandler(dbOperWrapper);
    }

    /**
     * 查询指定方法中指定被调用对象或参数对应的值
     *
     * @param callId     方法调用ID
     * @param objArgSeqs 需要查询的被调用对象或参数序号，若为空则查询全部被调用对象及参数，0代表被调用对象，1开始为参数
     * @return key: 被调用对象或参数序号 value: 对应的可能的值的列表
     */
    public Map<Integer, List<MethodCallObjArgValue>> queryMethodCallArgValuesSupportEnum(int callId, int... objArgSeqs) {
        Map<Integer, List<MethodCallObjArgValue>> map = new HashMap<>();
        List<WriteDbData4MethodCallInfo> methodCallInfoList = methodCallInfoHandler.queryMethodCallInfoObjArgs(callId, objArgSeqs);
        if (JavaCG2Util.isCollectionEmpty(methodCallInfoList)) {
            return map;
        }
        // 遍历处理每个参数的情况
        for (WriteDbData4MethodCallInfo methodCallInfo : methodCallInfoList) {
            MethodCallObjArgValue methodCallObjArgValue = getMethodCallObjArgValueSupportEnum(methodCallInfo);
            if (methodCallObjArgValue == null) {
                continue;
            }
            List<MethodCallObjArgValue> list = map.computeIfAbsent(methodCallInfo.getObjArgsSeq(), k -> new ArrayList<>());
            list.add(methodCallObjArgValue);
        }
        return map;
    }

    /**
     * 根据方法调用对象获取对应的对象或参数的值
     * 支持常量值、枚举常量方法调用（获取返回值）、枚举常量（获取名称）
     *
     * @param methodCallInfo
     * @return
     */
    private MethodCallObjArgValue getMethodCallObjArgValueSupportEnum(WriteDbData4MethodCallInfo methodCallInfo) {
        JavaCG2MethodCallInfoTypeEnum methodCallInfoTypeEnum = JavaCG2MethodCallInfoTypeEnum.getFromType(methodCallInfo.getType());
        MethodCallObjArgValue methodCallObjArgValue = new MethodCallObjArgValue();
        methodCallObjArgValue.setTypeEnum(methodCallInfoTypeEnum);
        String value = methodCallInfo.getTheValue();
        switch (methodCallInfoTypeEnum) {
            case MCIT_VALUE:
                // 当前数据类型属于值
                methodCallObjArgValue.setValueType(methodCallInfo.getValueType());
                methodCallObjArgValue.setValue(value);
                return methodCallObjArgValue;
            case MCIT_STATIC_FIELD_MCR:
                // 当前数据类型属于静态字段的方法调用返回值
                methodCallObjArgValue.setValueType(JavaCG2ConstantTypeEnum.CONSTTE_STRING.getType());
                String returnValue = queryCallEnumConstantFieldMethodReturnValue(value);
                if (StringUtils.isBlank(returnValue)) {
                    return null;
                }
                methodCallObjArgValue.setValue(returnValue);
                return methodCallObjArgValue;
            case MCIT_STATIC_FIELD:
                // 当前数据类型属于静态字段
                methodCallObjArgValue.setValueType(JavaCG2ConstantTypeEnum.CONSTTE_STRING.getType());
                methodCallObjArgValue.setValue(value);
                return methodCallObjArgValue;
            default:
                return null;
        }
    }

    /**
     * 查询调用的枚举常量的方法返回的值
     *
     * @param fieldMethodReturnValue
     * @return
     */
    private String queryCallEnumConstantFieldMethodReturnValue(String fieldMethodReturnValue) {
        ClassFieldMethodCall classFieldMethodCall = JavaCG2ClassMethodUtil.parseClassFieldMethodCall(fieldMethodReturnValue);
        String className = classFieldMethodCall.getClassName();
        Integer classAccessFlag = classInfoHandler.queryClassAccessFlag(className);
        if (classAccessFlag == null) {
            logger.warn("未查询到指定类的信息 {}", className);
            return null;
        }
        if (!JavaCG2ByteCodeUtil.isEnumFlag(classAccessFlag)) {
            logger.debug("被调用类不是枚举，不处理 {}", className);
            return null;
        }
        return enumsHandler.queryEnumConstantFieldMethodReturnValue(className, classFieldMethodCall.getFieldName(), classFieldMethodCall.genFullMethod(),
                classFieldMethodCall.getReturnType());
    }

    /**
     * 查询方法调用，及对应的被调用对象、参数的值（支持枚举）
     *
     * @param calleeClassName  被调用类名
     * @param calleeMethodName 被调用方法名称
     * @param objArgSeqs       需要查询的被调用对象或参数序号，若为空则查询全部被调用对象及参数，0代表被调用对象，1开始为参数
     * @return
     */
    public List<MethodCallWithValueSupportEnum> queryMethodCallWithValueByClassMethodSupportEnum(String calleeClassName, String calleeMethodName, int... objArgSeqs) {
        List<MethodCallWithValueSupportEnum> list = new ArrayList<>();
        List<WriteDbData4MethodCall> methodCallList = methodCallHandler.queryNormalMethodCallByCalleeClassMethod(calleeClassName, calleeMethodName, true);
        doQueryMethodCallWithValueSupportEnum(list, methodCallList, objArgSeqs);
        return list;
    }

    /**
     * 查询方法调用，及对应的被调用对象、参数的值（支持枚举）
     *
     * @param calleeFullMethod 被调用方完整方法
     * @param calleeReturnType 被调用方法返回类型，可为空
     * @param objArgSeqs       需要查询的被调用对象或参数序号，若为空则查询全部被调用对象及参数，0代表被调用对象，1开始为参数
     * @return
     */
    public List<MethodCallWithValueSupportEnum> queryMethodCallWithValueByMethodSupportEnum(String calleeFullMethod, String calleeReturnType, int... objArgSeqs) {
        List<MethodCallWithValueSupportEnum> list = new ArrayList<>();
        List<WriteDbData4MethodCall> methodCallList;
        if (StringUtils.isNotBlank(calleeReturnType)) {
            methodCallList = methodCallHandler.queryNormalMethodCallByCalleeMethodReturnType(calleeFullMethod, calleeReturnType);
        } else {
            methodCallList = methodCallHandler.queryNormalMethodCallByCalleeFullMethod(calleeFullMethod);
        }

        doQueryMethodCallWithValueSupportEnum(list, methodCallList, objArgSeqs);
        return list;
    }

    // 执行查询方法调用，及对应的被调用对象、参数的值（支持枚举）
    private void doQueryMethodCallWithValueSupportEnum(List<MethodCallWithValueSupportEnum> list, List<WriteDbData4MethodCall> methodCallList, int... objArgSeqs) {
        if (JavaCG2Util.isCollectionEmpty(methodCallList)) {
            return;
        }
        for (WriteDbData4MethodCall methodCall : methodCallList) {
            MethodCallWithValueSupportEnum methodCallWithValueSupportEnum = new MethodCallWithValueSupportEnum();
            methodCallWithValueSupportEnum.setMethodCall(methodCall);
            list.add(methodCallWithValueSupportEnum);
            Map<Integer, List<MethodCallObjArgValue>> map = queryMethodCallArgValuesSupportEnum(methodCall.getCallId(), objArgSeqs);
            if (!JavaCG2Util.isMapEmpty(map)) {
                methodCallWithValueSupportEnum.setMethodCallObjArgValueMap(map);
            }
        }
    }
}
