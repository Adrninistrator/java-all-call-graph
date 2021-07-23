package com.adrninistrator.jacg.util;

import com.adrninistrator.jacg.common.Constants;
import org.apache.commons.codec.binary.Base64;
import org.apache.commons.codec.digest.DigestUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Calendar;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2021/6/17
 * @description:
 */

public class CommonUtil {
    private static final Logger logger = LoggerFactory.getLogger(CommonUtil.class);

    private static Map<Integer, String> outputFlagMap = new HashMap<>();

    /**
     * 从完整类名中获取简单类名（去掉包名）
     *
     * @param fullClassName 完整类名
     * @return
     */
    public static String getSimpleClassNameFromFull(String fullClassName) {
        int indexLastDot = fullClassName.lastIndexOf(Constants.FLAG_DOT);
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
    public static String getMethodWithArgs(String method) {
        int indexColon = method.indexOf(Constants.FLAG_COLON);
        return method.substring(indexColon + 1);
    }

    /**
     * 从完整方法信息中获取方法名
     *
     * @param method 完整方法信息
     * @return
     */
    public static String getOnlyMethodName(String method) {
        int indexColon = method.indexOf(Constants.FLAG_COLON);
        int indexLeftBrackets = method.indexOf(Constants.FLAG_LEFT_BRACKET);
        return method.substring(indexColon + 1, indexLeftBrackets);
    }

    /**
     * 将方法名中的<替换为(，>替换为)，防止无法在Windows环境生成文件
     * 用于处理<init>、<clint>等方法
     *
     * @param methodName
     * @return
     */
    public static String getSafeMethodName(String methodName) {
        return methodName.replaceAll("<", "(")
                .replaceAll(">", ")");
    }

    /**
     * 从完整方法信息中获取完整类名
     *
     * @param method 完整方法信息
     * @return
     */
    public static String getFullClassNameFromMethod(String method) {
        int indexLastColon = method.lastIndexOf(Constants.FLAG_COLON);
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

    public static String genOutputFlag(int level) {
        if (level <= 0) {
            return "";
        }

        String flag = outputFlagMap.get(level);
        if (flag == null) {
            StringBuilder stringBuilder = new StringBuilder();
            for (int i = 0; i < level; i++) {
                stringBuilder.append(Constants.OUTPUT_SPLIT_FLAG);
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
        Calendar calendar = Calendar.getInstance();
        return String.format("%04d%02d%02d-%02d%02d%02d.%03d", calendar.get(Calendar.YEAR), calendar.get(Calendar.MONTH) + 1,
                calendar.get(Calendar.DATE),
                calendar.get(Calendar.HOUR_OF_DAY), calendar.get(Calendar.MINUTE), calendar.get(Calendar.SECOND), calendar.get(Calendar.MILLISECOND));
    }

    private CommonUtil() {
        throw new IllegalStateException("illegal");
    }
}
