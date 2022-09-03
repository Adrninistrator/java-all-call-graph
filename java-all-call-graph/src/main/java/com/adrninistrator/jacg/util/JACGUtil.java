package com.adrninistrator.jacg.util;

import com.adrninistrator.jacg.common.JACGConstants;
import org.apache.commons.codec.binary.Base64;
import org.apache.commons.codec.digest.DigestUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
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
     * @param fullMethod 完整方法信息
     * @return
     */
    public static String getMethodNameWithArgsFromFull(String fullMethod) {
        int indexColon = fullMethod.indexOf(JACGConstants.FLAG_COLON);
        return fullMethod.substring(indexColon + 1);
    }

    /**
     * 从完整方法信息中获取方法名
     *
     * @param fullMethod 完整方法信息
     * @return
     */
    public static String getMethodNameFromFull(String fullMethod) {
        int indexColon = fullMethod.indexOf(JACGConstants.FLAG_COLON);
        int indexLeftBrackets = fullMethod.indexOf(JACGConstants.FLAG_LEFT_BRACKET);
        return fullMethod.substring(indexColon + 1, indexLeftBrackets);
    }

    /**
     * 从完整方法信息中获取类名及方法名，删除括号及参数
     *
     * @param fullMethod 完整方法信息
     * @return
     */
    public static String getClassMethodNameFromFull(String fullMethod) {
        int indexLeftBrackets = fullMethod.indexOf(JACGConstants.FLAG_LEFT_BRACKET);
        return fullMethod.substring(0, indexLeftBrackets);
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
     * 从[完整或简单类名]@[方法名]@[方法HASH+长度]格式的文件名转换为[完整或简单类名]:[方法名]()格式的方法名
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

    /**
     * 为文件路径增加分隔符
     *
     * @param filePath
     * @return
     */
    public static String addSeparator4FilePath(String filePath) {
        if (StringUtils.endsWithAny(filePath, "/", "\\")) {
            // 文件路径以分隔符结尾，则直接使用
            return filePath;
        }

        // 文件路径没有以分隔符结尾，则在后面增加分隔符
        return filePath + File.separator;
    }

    /**
     * 获取JVM参数中指定的目录路径
     *
     * @param jvmOptionKey
     * @return
     */
    public static String getDirPathInJvmOptions(String jvmOptionKey) {
        String dirPath = System.getProperty(jvmOptionKey);
        if (dirPath == null) {
            return "";
        }

        return addSeparator4FilePath(dirPath);
    }

    /**
     * 判断字符串是否为数字
     *
     * @param str
     * @return
     */
    public static boolean isValidNum(String str) {
        if (str == null || str.isEmpty()) {
            return false;
        }

        for (char ch : str.toCharArray()) {
            if (ch > '9' || ch < '0') {
                return false;
            }
        }
        return true;
    }

    /**
     * 根据文件路径获取文件名
     *
     * @param filePath
     * @return
     */
    public static String getFileNameFromPath(String filePath) {
        if (filePath == null) {
            return null;
        }

        String tmpFilePath = replaceFilePathSeparator(filePath);
        int lastSeparatorIndex = tmpFilePath.lastIndexOf("/");

        if (lastSeparatorIndex == -1) {
            return filePath;
        }

        return tmpFilePath.substring(lastSeparatorIndex + 1);
    }

    /**
     * 获取不包含后缀的文件名
     *
     * @param fileName 文件名
     * @return
     */
    public static String getFileNameWithOutExt(String fileName) {
        return getFileNameWithOutExt(fileName, JACGConstants.FLAG_DOT);
    }


    /**
     * 获取不包含后缀的文件名
     *
     * @param fileName 文件名
     * @param fileExt  文件后缀，默认使用.
     * @return
     */
    public static String getFileNameWithOutExt(String fileName, String fileExt) {
        if (fileName == null || fileExt == null) {
            return null;
        }

        int lastDotIndex = fileName.lastIndexOf(fileExt);
        if (lastDotIndex == -1) {
            return fileName;
        }

        return fileName.substring(0, lastDotIndex);
    }

    /**
     * 将文件路径中的\替换为/
     *
     * @param filePath
     * @return
     */
    public static String replaceFilePathSeparator(String filePath) {
        return StringUtils.replace(filePath, "\\", "/");
    }

    /**
     * 获取文件所在目录名
     *
     * @param filePath
     * @return
     */
    public static String getFileParentDirName(String filePath) {
        if (filePath == null) {
            return null;
        }

        String tmpFilePath = replaceFilePathSeparator(filePath);
        String[] array = tmpFilePath.split("/");
        if (array.length < 2) {
            return null;
        }

        // 使用分隔符进行分隔后，返回倒数第二个数据
        return array[array.length - 2];
    }

    /**
     * 从字符串找到第1个非指定字符的下标，并生成子字符串
     *
     * @param data
     * @param flag
     * @return
     */
    public static String getFirstExcludeSubString(String data, char flag) {
        if (data == null) {
            return null;
        }

        for (int index = 0; index <= data.length(); index++) {
            if (data.charAt(index) != flag) {
                return data.substring(index);
            }
        }

        return null;
    }

    /**
     * 获取对字符串使用分隔符进行分隔后，第1列的数据
     *
     * @param data      字符串
     * @param separator 分隔符
     * @return
     */
    public static String getFirstSubString(String data, String separator) {
        if (data == null || separator == null) {
            return null;
        }

        int index = data.indexOf(separator);
        if (index == -1) {
            return data;
        }

        return data.substring(0, index);
    }

    /**
     * 获取简单类名首字母小写后的结果
     *
     * @param simpleClassName 简单类名
     * @return
     */
    public static String getFirstLetterLowerClassName(String simpleClassName) {
        if (simpleClassName == null) {
            return null;
        }

        if (simpleClassName.isEmpty()) {
            return "";
        }

        String firstLetterLower = simpleClassName.substring(0, 1).toLowerCase();
        if (simpleClassName.length() == 1) {
            return firstLetterLower;
        }

        return firstLetterLower + simpleClassName.substring(1);
    }

    private JACGUtil() {
        throw new IllegalStateException("illegal");
    }
}
