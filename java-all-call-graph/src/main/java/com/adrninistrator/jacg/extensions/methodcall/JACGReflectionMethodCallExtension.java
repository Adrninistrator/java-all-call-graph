package com.adrninistrator.jacg.extensions.methodcall;

import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.method.MethodDetailNoReturnType;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCall;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCallInfo;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodInfo;
import com.adrninistrator.jacg.handler.method.MethodInfoHandler;
import com.adrninistrator.jacg.handler.methodcall.MethodCallHandler;
import com.adrninistrator.jacg.handler.methodcall.MethodCallInfoHandler;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.javacg2.common.JavaCG2CommonNameConstants;
import com.adrninistrator.javacg2.common.enums.JavaCG2CallTypeEnum;
import com.adrninistrator.javacg2.common.enums.JavaCG2MethodCallInfoTypeEnum;
import com.adrninistrator.javacg2.util.JavaCG2ClassMethodUtil;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2026/02/13
 * @description: JACG 方法调用处理扩展类 - 反射调用
 * 替换占位内容为实际的反射调用方法
 */
public class JACGReflectionMethodCallExtension extends AbstractJACGMethodCallExtension {

    private static final Logger logger = LoggerFactory.getLogger(JACGReflectionMethodCallExtension.class);

    // 反射相关的方法名称
    private static final String METHOD_GET_METHOD = "getMethod";
    private static final String METHOD_GET_DECLARED_METHOD = "getDeclaredMethod";

    // Class类名
    private static final String CLASS_CLASS_NAME = Class.class.getName();

    private final MethodCallInfoHandler methodCallInfoHandler;
    private final MethodInfoHandler methodInfoHandler;
    private final MethodCallHandler methodCallHandler;

    public JACGReflectionMethodCallExtension(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
        methodCallInfoHandler = new MethodCallInfoHandler(dbOperWrapper);
        methodInfoHandler = new MethodInfoHandler(dbOperWrapper);
        methodCallHandler = new MethodCallHandler(dbOperWrapper);
    }

    @Override
    public String getCallType() {
        // 使用对应的 JavaCG2ObjInstanceMethodNameMCE 子类的 TYPE
        return JavaCG2ReflectionMethodCallExtension.TYPE;
    }

    @Override
    public boolean handle(WriteDbData4MethodCall methodCall) {
        // 获取对应的原始方法调用ID
        int rawMethodCallId = Integer.parseInt(methodCall.getDescription());
        // 修改方法调用类型及描述
        modifyMethodCall(methodCall, rawMethodCallId);

        // 追溯 Method 对象的来源
        MethodDetailNoReturnType methodDetail = traceMethodObject(rawMethodCallId);
        if (methodDetail == null) {
            logger.debug("无法追溯到Method对象的来源 callId: {}", rawMethodCallId);
            return true;
        }

        // 查找被调用的完整方法及返回类型
        WriteDbData4MethodInfo calleeMethodInfo = findCalleeMethodInfo(methodDetail.getClassName(), methodDetail.getMethodName(), methodDetail.getArgTypeList());

        String calleeClassName = methodDetail.getClassName();
        String calleeSimpleClassName = dbOperWrapper.querySimpleClassName(calleeClassName);
        String calleeFullMethod;
        String calleeReturnType;

        if (calleeMethodInfo != null) {
            // 找到对应的被调用方法
            calleeClassName = JavaCG2ClassMethodUtil.getClassNameFromMethod(calleeMethodInfo.getFullMethod());
            calleeSimpleClassName = dbOperWrapper.querySimpleClassName(calleeClassName);
            calleeFullMethod = calleeMethodInfo.getFullMethod();
            calleeReturnType = calleeMethodInfo.getReturnType();
        } else {
            // 未找到对应的被调用方法，说明被调用对象所在的jar可能未被解析
            // 使用解析到的类名、方法名、方法参数构造方法信息，返回类型使用Method.invoke在method_call表中对应记录的actual_return_type
            logger.info("未找到对应的被调用方法，使用解析到的信息 class: {}, method: {}", methodDetail.getClassName(), methodDetail.getMethodName());
            calleeFullMethod = JavaCG2ClassMethodUtil.formatFullMethodStr(
                    methodDetail.getClassName(),
                    methodDetail.getMethodName(),
                    methodDetail.getArgTypeList() != null ? methodDetail.getArgTypeList().toArray(new String[0]) : new String[0]);
            // 返回类型使用Method.invoke在method_call表中对应记录的actual_return_type（若为空或空字符串时使用void）
            String actualReturnType = methodCall.getActualReturnType();
            calleeReturnType = StringUtils.isNotBlank(actualReturnType) ? actualReturnType : JavaCG2CommonNameConstants.RETURN_TYPE_VOID;
        }

        methodCall.setCalleeSimpleClassName(calleeSimpleClassName);
        methodCall.setCalleeMethodName(methodDetail.getMethodName());
        methodCall.setCalleeFullMethod(calleeFullMethod);
        methodCall.setCalleeMethodHash(JACGClassMethodUtil.genMethodHashWithLen(calleeFullMethod, calleeReturnType));
        methodCall.setRawReturnType(calleeReturnType);
        methodCall.setActualReturnType(calleeReturnType);
        methodCall.setCalleeJarNum(0);
        return true;
    }

    // 修改方法调用类型及描述
    private void modifyMethodCall(WriteDbData4MethodCall methodCall, int rawMethodCallId) {
        methodCall.setCallType(JavaCG2CallTypeEnum.CTE_MANUAL_ADDED.getType());
        methodCall.setDescription("反射调用 " + rawMethodCallId);
    }

    /**
     * 追溯 Method 对象的来源
     *
     * @param invokeCallId Method.invoke 的方法调用ID
     * @return 方法详细信息
     */
    private MethodDetailNoReturnType traceMethodObject(int invokeCallId) {
        // 获取 Method 对象对应的 callId
        Integer methodObjectCallId = getMethodObjectCallId(invokeCallId);
        if (methodObjectCallId == null) {
            return null;
        }

        // 验证是否是 Class.getMethod 或 Class.getDeclaredMethod
        if (!isGetMethodCall(methodObjectCallId)) {
            return null;
        }

        // 获取方法名
        String methodName = getMethodNameFromCall(methodObjectCallId);
        if (methodName == null) {
            return null;
        }

        // 获取参数类型
        List<String> paramTypes = getMethodParamTypes(methodObjectCallId);
        if (paramTypes == null) {
            return null;
        }

        // 获取类名
        String className = getClassNameFromMethodCall(methodObjectCallId);
        if (className == null) {
            return null;
        }

        MethodDetailNoReturnType methodDetail = new MethodDetailNoReturnType();
        methodDetail.setClassName(className);
        methodDetail.setMethodName(methodName);
        methodDetail.setArgTypeList(paramTypes);

        return methodDetail;
    }

    /**
     * 获取 Method 对象对应的 callId
     */
    private Integer getMethodObjectCallId(int invokeCallId) {
        WriteDbData4MethodCallInfo methodCallInfo =
                methodCallInfoHandler.queryMethodCallInfoByCallIdSeqType(
                        invokeCallId, 0, 0, JavaCG2MethodCallInfoTypeEnum.MCIT_METHOD_CALL_RETURN_CALL_ID.getType());

        if (methodCallInfo == null) {
            logger.debug("未找到Method.invoke的对象对应的method_call_info callId: {}", invokeCallId);
            return null;
        }

        try {
            return Integer.parseInt(methodCallInfo.getTheValue());
        } catch (NumberFormatException e) {
            logger.error("解析Method.invoke的对象callId失败 value: {}", methodCallInfo.getTheValue());
            return null;
        }
    }

    /**
     * 验证是否是 Class.getMethod 或 Class.getDeclaredMethod 调用
     */
    private boolean isGetMethodCall(int methodObjectCallId) {
        String calleeFullMethod = methodCallHandler.queryCalleeFullMethodById(methodObjectCallId);
        if (StringUtils.isBlank(calleeFullMethod)) {
            logger.debug("未找到callId对应的方法 callId: {}", methodObjectCallId);
            return false;
        }

        String calleeClass = JavaCG2ClassMethodUtil.getClassNameFromMethod(calleeFullMethod);
        String calleeMethod = JavaCG2ClassMethodUtil.getMethodNameFromFull(calleeFullMethod);

        if (!CLASS_CLASS_NAME.equals(calleeClass)) {
            logger.debug("不是Class类的方法调用: {}", calleeFullMethod);
            return false;
        }

        if (!METHOD_GET_METHOD.equals(calleeMethod) && !METHOD_GET_DECLARED_METHOD.equals(calleeMethod)) {
            logger.debug("不是getMethod或getDeclaredMethod方法: {}", calleeFullMethod);
            return false;
        }

        return true;
    }

    /**
     * 从方法调用中获取方法名
     */
    private String getMethodNameFromCall(int methodObjectCallId) {
        List<String> methodNames = methodCallInfoHandler.queryMethodCallObjArgValues(methodObjectCallId, 1);
        if (JavaCG2Util.isCollectionEmpty(methodNames)) {
            return null;
        }

        if (methodNames.size() > 1) {
            logger.debug("获取到多个方法名，存在多种情况 callId: {}", methodObjectCallId);
            return null;
        }

        return methodNames.get(0);
    }

    /**
     * 从方法调用中获取参数类型列表
     */
    private List<String> getMethodParamTypes(int methodObjectCallId) {
        List<Map<Integer, String>> typeList = methodCallInfoHandler.queryMethodCallArrayElementTypes(methodObjectCallId, 2);

        if (JavaCG2Util.isCollectionEmpty(typeList)) {
            // 参数2不存在，说明通过反射调用的方法参数为空
            return Collections.emptyList();
        }

        if (typeList.size() > 1) {
            logger.warn("参数2获取到多个参数类型组合，存在多种情况 callId: {}", methodObjectCallId);
            return null;
        }

        Map<Integer, String> typeMap = typeList.get(0);
        List<String> result = new ArrayList<>();
        if (!typeMap.isEmpty()) {
            List<Integer> indexList = new ArrayList<>(typeMap.keySet());
            Collections.sort(indexList);
            for (Integer index : indexList) {
                result.add(typeMap.get(index));
            }
        }
        return result;
    }

    /**
     * 从方法调用中获取类名
     */
    private String getClassNameFromMethodCall(int methodObjectCallId) {
        // 获取 Class.forName 调用的类名
        WriteDbData4MethodCallInfo classObjectCallInfo =
                methodCallInfoHandler.queryMethodCallInfoByCallIdSeqType(
                        methodObjectCallId, 0, 0, JavaCG2MethodCallInfoTypeEnum.MCIT_METHOD_CALL_RETURN_CALL_ID.getType());

        if (classObjectCallInfo == null) {
            logger.debug("未获取到类对象类型信息 callId: {}", methodObjectCallId);
            return null;
        }

        int classForNameCallId;
        try {
            classForNameCallId = Integer.parseInt(classObjectCallInfo.getTheValue());
        } catch (NumberFormatException e) {
            logger.error("解析类对象来源callId失败 value: {}", classObjectCallInfo.getTheValue());
            return null;
        }

        // 验证是否是 Class.forName 调用
        if (!isForNameCall(classForNameCallId)) {
            return null;
        }

        // 获取 Class.forName 的参数（类名）
        List<String> classNames = methodCallInfoHandler.queryMethodCallObjArgValues(classForNameCallId, 1);
        if (JavaCG2Util.isCollectionEmpty(classNames)) {
            return null;
        }

        if (classNames.size() > 1) {
            logger.debug("获取到多个类名，存在多种情况 callId: {}", classForNameCallId);
            return null;
        }

        return classNames.get(0);
    }

    /**
     * 验证是否是 Class.forName 调用
     */
    private boolean isForNameCall(int callId) {
        String calleeFullMethod = methodCallHandler.queryCalleeFullMethodById(callId);
        if (StringUtils.isBlank(calleeFullMethod)) {
            logger.debug("未找到类对象来源的方法调用 callId: {}", callId);
            return false;
        }

        String forNameClassName = JavaCG2ClassMethodUtil.getClassNameFromMethod(calleeFullMethod);
        String forNameMethodName = JavaCG2ClassMethodUtil.getMethodNameFromFull(calleeFullMethod);

        if (!CLASS_CLASS_NAME.equals(forNameClassName) || !"forName".equals(forNameMethodName)) {
            logger.debug("类对象来源不是 Class.forName 调用: {}", calleeFullMethod);
            return false;
        }

        return true;
    }

    /**
     * 查找被调用的完整方法及返回类型
     */
    private WriteDbData4MethodInfo findCalleeMethodInfo(String className, String methodName, List<String> paramTypes) {
        List<WriteDbData4MethodInfo> methodInfoList = methodInfoHandler.queryMethodByClassMethodUpper(className, methodName);

        if (JavaCG2Util.isCollectionEmpty(methodInfoList)) {
            return null;
        }

        WriteDbData4MethodInfo matchedMethodInfo = null;

        if (methodInfoList.size() == 1) {
            matchedMethodInfo = methodInfoList.get(0);
        } else {
            // 根据参数类型匹配
            if (JavaCG2Util.isCollectionEmpty(paramTypes)) {
                for (WriteDbData4MethodInfo methodInfo : methodInfoList) {
                    if (methodInfo.getFullMethod().endsWith("()")) {
                        matchedMethodInfo = methodInfo;
                        break;
                    }
                }
            } else {
                for (WriteDbData4MethodInfo methodInfo : methodInfoList) {
                    if (matchMethodArguments(methodInfo.getFullMethod(), paramTypes)) {
                        matchedMethodInfo = methodInfo;
                        break;
                    }
                }
            }
        }

        return matchedMethodInfo;
    }

    /**
     * 匹配方法参数
     */
    private boolean matchMethodArguments(String fullMethod, List<String> paramTypes) {
        MethodDetailNoReturnType methodDetail = JACGClassMethodUtil.genMethodDetailNoReturnType(fullMethod);
        List<String> dbArgTypeList = methodDetail.getArgTypeList();

        if (JavaCG2Util.isCollectionEmpty(dbArgTypeList)) {
            return JavaCG2Util.isCollectionEmpty(paramTypes);
        }

        if (JavaCG2Util.isCollectionEmpty(paramTypes)) {
            return false;
        }

        if (dbArgTypeList.size() != paramTypes.size()) {
            return false;
        }

        for (int i = 0; i < dbArgTypeList.size(); i++) {
            String dbArgType = dbArgTypeList.get(i);
            String reflectArgType = paramTypes.get(i);

            if (StringUtils.equals(dbArgType, reflectArgType)) {
                continue;
            }

            String normalizedDbType = normalizeType(dbArgType);
            String normalizedReflectType = normalizeType(reflectArgType);

            if (!StringUtils.equals(normalizedDbType, normalizedReflectType)) {
                return false;
            }
        }

        return true;
    }

    /**
     * 规范化类型名称
     */
    private String normalizeType(String type) {
        if (StringUtils.isBlank(type)) {
            return type;
        }

        if (type.startsWith("java.lang.")) {
            return type.substring("java.lang.".length());
        }

        return type;
    }
}
