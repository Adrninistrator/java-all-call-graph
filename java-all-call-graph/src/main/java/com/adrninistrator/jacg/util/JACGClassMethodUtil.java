package com.adrninistrator.jacg.util;

import com.adrninistrator.jacg.dto.method.ClassAndMethodName;
import com.adrninistrator.jacg.dto.method.MethodDetail;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCall;
import com.adrninistrator.jacg.handler.common.enums.ClassInterfaceEnum;
import com.adrninistrator.javacg2.common.JavaCG2CommonNameConstants;
import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.common.enums.JavaCG2CallTypeEnum;
import com.adrninistrator.javacg2.exceptions.JavaCG2RuntimeException;
import com.adrninistrator.javacg2.util.JavaCG2ByteCodeUtil;
import com.adrninistrator.javacg2.util.JavaCG2ClassMethodUtil;
import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2022/11/19
 * @description: 类名与方法处理工具类
 */
public class JACGClassMethodUtil {

    /**
     * 将完整方法与返回类型拼接
     *
     * @param fullMethod
     * @param returnType
     * @return
     */
    public static String genFullMethodWithReturnType(String fullMethod, String returnType) {
        return fullMethod + JavaCG2Constants.FLAG_COLON + returnType;
    }

    /**
     * 从完整类名中获取包名
     *
     * @param className
     * @return 完整类名
     */
    public static String getPackageNameFromFullClassName(String className) {
        return StringUtils.substringBeforeLast(className, JavaCG2Constants.FLAG_DOT);
    }

    /**
     * 从完整方法信息中获取完整类名
     *
     * @param method 完整方法信息
     * @return
     */
    public static String getClassNameFromMethod(String method) {
        return StringUtils.substringBeforeLast(method, JavaCG2Constants.FLAG_COLON);
    }

    /**
     * 从完整方法信息中获取简单类名（去掉包名）
     *
     * @param method 完整方法信息
     * @return
     */
    public static String getSimpleClassNameFromMethod(String method) {
        String className = getClassNameFromMethod(method);
        return JavaCG2ClassMethodUtil.getSimpleClassNameFromFull(className);
    }

    /**
     * 从完整方法信息中获取方法名+参数（去掉类名）
     *
     * @param fullMethod 完整方法信息
     * @return
     */
    public static String getMethodNameWithArgsFromFull(String fullMethod) {
        return StringUtils.substringAfter(fullMethod, JavaCG2Constants.FLAG_COLON);
    }

    /**
     * 从完整方法信息中获取方法参数类型，不包含括号
     *
     * @param fullMethod 完整方法信息
     * @return
     */
    public static String getMethodArgTypes(String fullMethod) {
        return StringUtils.substringBetween(fullMethod, JavaCG2Constants.FLAG_LEFT_BRACKET, JavaCG2Constants.FLAG_RIGHT_BRACKET);
    }

    /**
     * 从完整方法信息中获取方法名
     *
     * @param fullMethod 完整方法信息
     * @return
     */
    public static String getMethodNameFromFull(String fullMethod) {
        return StringUtils.substringBetween(fullMethod, JavaCG2Constants.FLAG_COLON, JavaCG2Constants.FLAG_LEFT_BRACKET);
    }

    /**
     * 从完整方法信息中获取类名及方法名，删除括号及参数
     *
     * @param fullMethod 完整方法信息
     * @return
     */
    public static String getClassMethodNameFromFull(String fullMethod) {
        return StringUtils.substringBefore(fullMethod, JavaCG2Constants.FLAG_LEFT_BRACKET);
    }

    /**
     * 将方法名中的<替换为(，>替换为)，防止无法在Windows环境生成文件
     * 用于处理<init>、<clint>等方法
     *
     * @param methodName
     * @return
     */
    public static String getSafeMethodName(String methodName) {
        if (methodName == null) {
            return null;
        }
        return methodName.replace("<", "(")
                .replace(">", ")");
    }

    /**
     * 恢复方法名中被替换的字符
     *
     * @param safeMethodName
     * @return
     */
    public static String recoveryMethodName(String safeMethodName) {
        if (safeMethodName == null) {
            return null;
        }
        return safeMethodName.replace("(", "<")
                .replace(")", ">");
    }

    /**
     * 生成方法详细信息
     *
     * @param fullMethod
     * @return
     */
    public static MethodDetail genMethodDetail(String fullMethod) {
        String className = getClassNameFromMethod(fullMethod);
        int indexColon = fullMethod.indexOf(JavaCG2Constants.FLAG_COLON);
        int indexLeftBrackets = fullMethod.indexOf(JavaCG2Constants.FLAG_LEFT_BRACKET);
        String methodName = fullMethod.substring(indexColon + 1, indexLeftBrackets);
        int indexRightBrackets = fullMethod.lastIndexOf(JavaCG2Constants.FLAG_RIGHT_BRACKET);
        // 不包含括号的方法参数类型字符串
        String argTypeStr = fullMethod.substring(indexLeftBrackets + JavaCG2Constants.FLAG_LEFT_BRACKET.length(), indexRightBrackets);
        return new MethodDetail(fullMethod, className, methodName, argTypeStr, genMethodArgTypeList(fullMethod));
    }

    /**
     * 根据完整方法生成方法参数类型列表
     *
     * @param fullMethod
     * @return
     */
    public static List<String> genMethodArgTypeList(String fullMethod) {
        String[] argTypes = StringUtils.splitPreserveAllTokens(getMethodArgTypes(fullMethod), JavaCG2Constants.FLAG_COMMA);
        return Arrays.asList(argTypes);
    }

    /**
     * 拼接类名与方法名
     * 格式：类名:方法名
     *
     * @param className
     * @param methodName
     * @return
     */
    public static String genClassAndMethodName(String className, String methodName) {
        return className + JavaCG2Constants.FLAG_COLON + methodName;
    }

    /**
     * 根据[类名]:[方法名]格式的字符串获取对应的类名与方法名对象
     *
     * @param methodInfo
     * @return
     */
    public static ClassAndMethodName parseClassAndMethodName(String methodInfo) {
        String[] array = StringUtils.splitPreserveAllTokens(methodInfo, JavaCG2Constants.FLAG_COLON);
        if (array == null || array.length != 2) {
            throw new JavaCG2RuntimeException("指定的字符串不满足[类名]:[方法名]格式 " + methodInfo);
        }
        return new ClassAndMethodName(array[0], array[1]);
    }

    /**
     * 生成get方法对应的set方法名
     *
     * @param getMethodName
     * @return
     */
    public static String genSetMethodName4GetMethod(String getMethodName) {
        return "s" + getMethodName.substring(1);
    }

    /**
     * 生成set方法对应的get方法名
     *
     * @param getMethodName
     * @return
     */
    public static String genGetMethodName4SetMethod(String getMethodName) {
        return "g" + getMethodName.substring(1);
    }

    /**
     * 判断方法调用中被调用方法是否匹配get方法
     *
     * @param methodCall
     * @return
     */
    public static boolean calleeMatchesGetMethod(WriteDbData4MethodCall methodCall) {
        if (!JavaCG2ClassMethodUtil.matchesGetMethod(methodCall.getCalleeMethodName()) ||
                JavaCG2CommonNameConstants.RETURN_TYPE_VOID.equals(methodCall.getRawReturnType()) ||
                JavaCG2CallTypeEnum.CTE_RAW_INVOKE_STATIC.getType().equals(methodCall.getCallType())) {
            return false;
        }
        List<String> argTypeList = genMethodArgTypeList(methodCall.getCalleeFullMethod());
        return argTypeList.isEmpty();
    }

    /**
     * 判断方法调用中被调用方法是否匹配set方法
     *
     * @param methodCall
     * @return
     */
    public static boolean calleeMatchesSetMethod(WriteDbData4MethodCall methodCall) {
        if (!JavaCG2ClassMethodUtil.matchesSetMethod(methodCall.getCalleeMethodName())) {
            return false;
        }
        // 在这里不判断方法返回类型，因为有的set方法返回类型非void
        List<String> argTypeList = genMethodArgTypeList(methodCall.getCalleeFullMethod());
        return argTypeList.size() == 1;
    }

    /**
     * 将字符串形式的类名与方法名数组转换为对象列表形式
     *
     * @param classMethodStringArray 类名与方法名数组
     *                               格式：[类名]:[方法名]
     *                               示例：org.slf4j.Logger:error org.slf4j.Logger:info org.slf4j.Logger:warn
     * @return
     */
    public static List<ClassAndMethodName> genClassAndMethodNameListFromString(String[] classMethodStringArray) {
        List<ClassAndMethodName> classAndMethodNameList = new ArrayList<>(classMethodStringArray.length);
        for (String expectedMethod : classMethodStringArray) {
            classAndMethodNameList.add(JACGClassMethodUtil.parseClassAndMethodName(expectedMethod));
        }
        return classAndMethodNameList;
    }

    /**
     * 检查方法是否在方法列表中
     *
     * @param fullMethod
     * @param classAndMethodNameList
     * @return true: 在 false: 不在
     */
    public static boolean checkMethodInList(String fullMethod, List<ClassAndMethodName> classAndMethodNameList) {
        String className = JACGClassMethodUtil.getClassNameFromMethod(fullMethod);
        String methodName = JACGClassMethodUtil.getMethodNameFromFull(fullMethod);
        for (ClassAndMethodName classAndMethodName : classAndMethodNameList) {
            if (className.equals(classAndMethodName.getClassName()) && methodName.equals(classAndMethodName.getMethodName())) {
                return true;
            }
        }
        return false;
    }

    /**
     * 根据accessFlags获得类名接口的类型
     *
     * @param accessFlags
     * @return
     */
    public static ClassInterfaceEnum getClassInterfaceEnum(Integer accessFlags) {
        if (accessFlags != null) {
            if (JavaCG2ByteCodeUtil.isInterfaceFlag(accessFlags)) {
                return ClassInterfaceEnum.CIE_INTERFACE;
            }
            if (JavaCG2ByteCodeUtil.isAbstractFlag(accessFlags)) {
                return ClassInterfaceEnum.CIE_ABSTRACT_CLASS;
            }
        }
        return ClassInterfaceEnum.CIE_CLASS;
    }

    private JACGClassMethodUtil() {
        throw new IllegalStateException("illegal");
    }
}
