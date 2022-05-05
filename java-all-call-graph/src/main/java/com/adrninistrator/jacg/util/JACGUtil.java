package com.adrninistrator.jacg.util;

import com.adrninistrator.jacg.common.JACGConstants;
import org.apache.commons.codec.binary.Base64;
import org.apache.commons.codec.digest.DigestUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.text.SimpleDateFormat;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2021/6/17
 * @description:
 */

public class JACGUtil {
    private static final Logger logger = LoggerFactory.getLogger(JACGUtil.class);

    private static Map<Integer, String> outputFlagMap = new HashMap<>();

    /**
     * 从完整类名中获取简单类名（去掉包名）
     *
     * @param fullClassName 完整类名
     * @return
     */
    public static String getSimpleClassNameFromFull(String fullClassName) {
        int indexLastDot = fullClassName.lastIndexOf(JACGConstants.FLAG_DOT);
        if (indexLastDot == -1) {
            return fullClassName;
        }
        return fullClassName.substring(indexLastDot + 1);
    }

    /**
     * 从完整方法信息中获取方法名+参数（去掉类名）
     *
     * @param method 完整方法信息
     * @return
     */
    public static String getMethodNameWithArgs(String method) {
        int indexColon = method.indexOf(JACGConstants.FLAG_COLON);
        return method.substring(indexColon + 1);
    }

    /**
     * 从完整方法信息中获取方法名
     *
     * @param method 完整方法信息
     * @return
     */
    public static String getOnlyMethodName(String method) {
        int indexColon = method.indexOf(JACGConstants.FLAG_COLON);
        int indexLeftBrackets = method.indexOf(JACGConstants.FLAG_LEFT_BRACKET);
        return method.substring(indexColon + 1, indexLeftBrackets);
    }

    /**
     * 将方法名中的<替换为(，>替换为)，防止无法在Windows环境生成文件
     * 用于处理<init>、<clint>等方法
     *
     * @param methodName
     * @return
     */
    public static String getSafeFileName(String methodName) {
        return methodName.replace("<", "(")
                .replace(">", ")");
    }

    /**
     * 从完整方法信息中获取完整类名
     *
     * @param method 完整方法信息
     * @return
     */
    public static String getFullClassNameFromMethod(String method) {
        int indexLastColon = method.lastIndexOf(JACGConstants.FLAG_COLON);
        return method.substring(0, indexLastColon);
    }


    public static boolean isInnerAnonymousClass(String className) {
        if (!className.contains("$")) {
            return false;
        }

        String[] array = className.split("\\$");
        if (array.length != 2) {
            return false;
        }

        if (!isNumStr(array[1])) {
            return false;
        }
        return true;
    }

    public static boolean isNumStr(String str) {
        char[] charArray = str.toCharArray();
        for (char ch : charArray) {
            if (ch < '0' || ch > '9') {
                return false;
            }
        }
        return true;
    }

    public static String genHashWithLen(String data) {
        byte[] md5 = DigestUtils.md5(data);
        // 以下使用的BASE64方法输出结果范围为字母+“+”+“/”，不是原始的字母+“-”+“_”
        return String.format("%s#%03x", Base64.encodeBase64URLSafeString(md5), data.length());
    }

    public static boolean isCollectionEmpty(Collection collection) {
        return collection == null || collection.isEmpty();
    }

    public static boolean isMapEmpty(Map map) {
        return map == null || map.isEmpty();
    }

    public static String genOutputFlag(int level) {
        if (level <= 0) {
            return "";
        }

        String flag = outputFlagMap.get(level);
        if (flag == null) {
            StringBuilder stringBuilder = new StringBuilder();
            for (int i = 0; i < level; i++) {
                stringBuilder.append(JACGConstants.OUTPUT_SPLIT_FLAG);
            }
            flag = stringBuilder.toString();
            outputFlagMap.put(level, flag);
        }

        return flag;
    }

    public static void sleep(long time) {
        try {
            Thread.sleep(time);
        } catch (InterruptedException e) {
            logger.error("error ", e);
            Thread.currentThread().interrupt();
        }
    }

    public static String currentTime() {
        SimpleDateFormat sdf = new SimpleDateFormat("yyyyMMdd-HHmmss.SSS");
        return sdf.format(new Date());
    }

    /**
     * 从生成的向下方法完整调用链的某行中获取对应的被调用类名（有显示调用者代码行号信息）
     *
     * @param line 指定行的内容
     * @return
     */
    public static String getCalleeClassNameWithCaller(String line) {
        if (line == null) {
            logger.error("指定的字符串为空");
            return null;
        }

        int tabIndex = line.indexOf(JACGConstants.FLAG_TAB);
        if (tabIndex == -1) {
            logger.error("指定的字符串不包含 {}", JACGConstants.FLAG_TAB);
            return null;
        }

        int colonAfterTabIndex = line.indexOf(JACGConstants.FLAG_COLON, tabIndex);
        if (colonAfterTabIndex == -1) {
            logger.error("指定的字符串 {} 之后不包含 {}", JACGConstants.FLAG_TAB, JACGConstants.FLAG_COLON);
            return null;
        }

        return line.substring(tabIndex + JACGConstants.FLAG_TAB.length(), colonAfterTabIndex);
    }

    /**
     * 从[完整或简单类名]@[方法名]@[方法HASH]格式的文件名转换为[完整或简单类名]:[方法名]()格式的方法名
     *
     * @param fileName
     * @return
     */
    public static String getMethodNameFromFileName(String fileName) {
        if (fileName == null) {
            return "";
        }

        int firstAtIndex = fileName.indexOf(JACGConstants.FLAG_AT);
        if (firstAtIndex == -1) {
            return fileName;
        }

        int secondAtIndex = fileName.indexOf(JACGConstants.FLAG_AT, firstAtIndex + JACGConstants.FLAG_AT.length());
        if (secondAtIndex == -1) {
            return fileName;
        }

        String className = fileName.substring(0, firstAtIndex);
        String methodName = fileName.substring(firstAtIndex + JACGConstants.FLAG_AT.length(), secondAtIndex);

        return className + JACGConstants.FLAG_COLON + methodName + JACGConstants.FLAG_LEFT_BRACKET + JACGConstants.FLAG_RIGHT_BRACKET;
    }

    /**
     * 根据类名获取对应实例
     *
     * @param className
     * @param classType
     * @param <T>
     * @return
     */
    public static <T> T getClassObject(String className, Class<T> classType) {
        try {
            Class clazz = Class.forName(className);
            Object obj = clazz.newInstance();

            if (!classType.isAssignableFrom(clazz)) {
                logger.error("指定的类 {} 不是 {} 的实现类", className, classType.getName());
                return null;
            }

            return (T) obj;
        } catch (Exception e) {
            logger.error("根据指定类名 {} 获得 {} 类的实例异常 ", className, classType.getName(), e);
            return null;
        }
    }

    public static String getAnnotationArrayAttributeValue(String value) {
        if (!StringUtils.startsWith(value, JACGConstants.FLAG_LEFT_BIG_PARENTHESES) || !StringUtils.endsWith(value, JACGConstants.FLAG_RIGHT_BIG_PARENTHESES)) {
            return value;
        }

        return value.substring(JACGConstants.FLAG_LEFT_BIG_PARENTHESES.length(), value.length() - JACGConstants.FLAG_RIGHT_BIG_PARENTHESES.length());
    }

    private JACGUtil() {
        throw new IllegalStateException("illegal");
    }
}
