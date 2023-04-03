package com.adrninistrator.jacg.util;

import com.adrninistrator.jacg.dto.method.MethodDetail;
import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.util.JavaCGUtil;
import org.apache.commons.lang3.StringUtils;

/**
 * @author adrninistrator
 * @date 2022/11/19
 * @description: 类名与方法处理工具类
 */
public class JACGClassMethodUtil {

    /**
     * 从完整方法信息中获取完整类名
     *
     * @param method 完整方法信息
     * @return
     */
    public static String getClassNameFromMethod(String method) {
        return StringUtils.substringBeforeLast(method, JavaCGConstants.FLAG_COLON);
    }

    /**
     * 从完整类名中获取简单类名（去掉包名）
     *
     * @param className 完整类名
     * @return
     */
    public static String getSimpleClassNameFromFull(String className) {
        int indexLastDot = className.lastIndexOf(JavaCGConstants.FLAG_DOT);
        if (indexLastDot == -1) {
            return className;
        }
        return className.substring(indexLastDot + 1);
    }

    /**
     * 从完整方法信息中获取简单类名（去掉包名）
     *
     * @param method 完整方法信息
     * @return
     */
    public static String getSimpleClassNameFromMethod(String method) {
        String className = getClassNameFromMethod(method);
        return getSimpleClassNameFromFull(className);
    }

    /**
     * 从完整方法信息中获取方法名+参数（去掉类名）
     *
     * @param fullMethod 完整方法信息
     * @return
     */
    public static String getMethodNameWithArgsFromFull(String fullMethod) {
        return StringUtils.substringAfter(fullMethod, JavaCGConstants.FLAG_COLON);
    }

    /**
     * 从完整方法信息中获取方法名
     *
     * @param fullMethod 完整方法信息
     * @return
     */
    public static String getMethodNameFromFull(String fullMethod) {
        return StringUtils.substringBetween(fullMethod, JavaCGConstants.FLAG_COLON, JavaCGConstants.FLAG_LEFT_BRACKET);
    }

    /**
     * 从完整方法信息中获取类名及方法名，删除括号及参数
     *
     * @param fullMethod 完整方法信息
     * @return
     */
    public static String getClassMethodNameFromFull(String fullMethod) {
        return StringUtils.substringBefore(fullMethod, JavaCGConstants.FLAG_LEFT_BRACKET);
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
     * 判断是否为匿名内部类
     *
     * @param className
     * @return
     */
    public static boolean isAnonymousInnerClass(String className) {
        if (!className.contains("$")) {
            return false;
        }

        String[] array = StringUtils.splitPreserveAllTokens(className, "$");
        if (array.length != 2) {
            return false;
        }

        if (!JavaCGUtil.isNumStr(array[1])) {
            return false;
        }
        return true;
    }

    /**
     * 生成方法详细信息
     *
     * @param fullMethod
     * @return
     */
    public static MethodDetail genMethodDetail(String fullMethod) {
        String className = getClassNameFromMethod(fullMethod);
        int indexColon = fullMethod.indexOf(JavaCGConstants.FLAG_COLON);
        int indexLeftBrackets = fullMethod.indexOf(JavaCGConstants.FLAG_LEFT_BRACKET);
        String methodName = fullMethod.substring(indexColon + 1, indexLeftBrackets);
        String argStr = fullMethod.substring(indexLeftBrackets);
        int indexRightBrackets = fullMethod.lastIndexOf(JavaCGConstants.FLAG_RIGHT_BRACKET);
        String argWithoutBrackets = fullMethod.substring(indexLeftBrackets + JavaCGConstants.FLAG_LEFT_BRACKET.length(), indexRightBrackets);
        String[] args = StringUtils.splitPreserveAllTokens(argWithoutBrackets, JavaCGConstants.FLAG_COMMA);
        return new MethodDetail(fullMethod, className, methodName, argStr, args);
    }

    /**
     * 拼接类名与方法名
     * 格式：类名:方法名
     *
     * @param className
     * @param methodName
     * @return
     */
    public static String getClassAndMethodName(String className, String methodName) {
        return className + JavaCGConstants.FLAG_COLON + methodName;
    }

    private JACGClassMethodUtil() {
        throw new IllegalStateException("illegal");
    }
}
