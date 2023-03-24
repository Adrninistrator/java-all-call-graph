package com.adrninistrator.jacg.util;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.dto.write_db.WriteDbData4MethodCall;
import com.adrninistrator.javacg.common.enums.JavaCGConfigKeyEnum;
import com.adrninistrator.javacg.conf.JavaCGConfigureWrapper;
import com.adrninistrator.javacg.exceptions.JavaCGRuntimeException;
import com.adrninistrator.javacg.util.JavaCGUtil;
import org.apache.commons.codec.digest.DigestUtils;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Base64;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ThreadPoolExecutor;

/**
 * @author adrninistrator
 * @date 2021/6/17
 * @description:
 */

public class JACGUtil {
    private static final Logger logger = LoggerFactory.getLogger(JACGUtil.class);

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
        // 以下使用的BASE64方法输出结果范围为字母+"-"+"_"，不是原始的字母+"+"+"/"
        return String.format("%s#%03x", Base64.getUrlEncoder().encodeToString(md5), data.length());
    }

    public static <K, V> boolean isMapEmpty(Map<K, V> map) {
        return map == null || map.isEmpty();
    }

    public static void sleep(long time) {
        try {
            Thread.sleep(time);
        } catch (InterruptedException e) {
            logger.error("error ", e);
            Thread.currentThread().interrupt();
        }
    }

    /**
     * 根据类名获取对应实例
     *
     * @param className
     * @param classType
     * @param <T>
     * @return
     */
    @SuppressWarnings("unchecked")
    public static <T> T getClassObject(String className, Class<T> classType) {
        try {
            Class<?> clazz = Class.forName(className);
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

    /**
     * 等待直到允许任务执行
     *
     * @param threadPoolExecutor
     * @param taskQueueMaxSize
     */
    public static void wait4TPEExecute(ThreadPoolExecutor threadPoolExecutor, int taskQueueMaxSize) {
        while (true) {
            if (threadPoolExecutor.getQueue().size() < taskQueueMaxSize) {
                return;
            }
            logger.debug("wait4TPEExecute ...");
            JACGUtil.sleep(100L);
        }
    }

    /**
     * 生成方法调用关系对象对应的数组
     *
     * @param data
     * @return
     */
    public static Object[] genMethodCallObjectArray(WriteDbData4MethodCall data) {
        return new Object[]{
                data.getCallId(),
                data.getCallType(),
                data.getCalleeObjType(),
                data.getEnabled(),
                data.getCallerJarNum(),
                data.getCallerMethodHash(),
                data.getCallerSimpleClassName(),
                data.getCallerMethodName(),
                data.getCallerFullMethod(),
                data.getCallerLineNumber(),
                data.getCalleeMethodHash(),
                data.getCalleeSimpleClassName(),
                data.getCalleeMethodName(),
                data.getCalleeFullMethod(),
                data.getCallFlags(),
                data.getRawReturnType(),
                data.getActualReturnType()
        };
    }

    /**
     * 将源列表中的元素添加到目标列表中
     * 忽略空的元素，忽略重复的元素
     *
     * @param srcList
     * @param destList
     */
    public static void addList2List(List<String> srcList, List<String> destList) {
        if (srcList == null || destList == null) {
            throw new JavaCGRuntimeException("传入参数不允许为空");
        }
        for (String src : srcList) {
            if (StringUtils.isBlank(src) || destList.contains(src)) {
                continue;
            }
            destList.add(src);
        }
    }

    /**
     * 在字符串列表中查找字符串
     *
     * @param stringList 字符串列表
     * @param strings    需要判断的字符串
     * @return 在字符串列表中找到的字符串
     */
    public static String findStringInList(List<String> stringList, String... strings) {
        if (JavaCGUtil.isCollectionEmpty(stringList) || ArrayUtils.isEmpty(strings)) {
            return null;
        }
        for (String string : strings) {
            if (stringList.contains(string)) {
                return string;
            }
        }
        return null;
    }

    /**
     * 根据不定长数组生成HashSet
     *
     * @param a
     * @param <T>
     * @return
     */
    @SafeVarargs
    public static <T> Set<T> genSetFromArray(T... a) {
        if (ArrayUtils.isEmpty(a)) {
            return new HashSet<>();
        }
        return new HashSet<>(Arrays.asList(a));
    }

    /**
     * 根据不定长数组生成List
     *
     * @param a
     * @param <T>
     * @return
     */
    @SafeVarargs
    public static <T> List<T> genListFromArray(T... a) {
        if (ArrayUtils.isEmpty(a)) {
            return new ArrayList<>();
        }
        return Arrays.asList(a);
    }

    /**
     * 获取变长参数中指定下标的参数，相关的参数都要使用包装类型，避免拆箱时产生空指针异常
     *
     * @param index
     * @param args
     * @param <T>
     * @return
     */
    @SuppressWarnings("unchecked")
    public static <T> T getArgAt(int index, Object... args) {
        if (ArrayUtils.isEmpty(args) || index >= args.length) {
            return null;
        }

        return (T) args[index];
    }

    /**
     * 生成java-callgraph2的配置
     *
     * @return
     */
    public static JavaCGConfigureWrapper genJavaCGConfigureWrapper() {
        JavaCGConfigureWrapper javaCGConfigureWrapper = new JavaCGConfigureWrapper();
        javaCGConfigureWrapper.setConfig(JavaCGConfigKeyEnum.CKE_PARSE_METHOD_CALL_TYPE_VALUE, Boolean.TRUE.toString());
        javaCGConfigureWrapper.setConfig(JavaCGConfigKeyEnum.CKE_FIRST_PARSE_INIT_METHOD_TYPE, Boolean.TRUE.toString());
        javaCGConfigureWrapper.setConfig(JavaCGConfigKeyEnum.CKE_CONTINUE_WHEN_ERROR, Boolean.FALSE.toString());
        javaCGConfigureWrapper.setConfig(JavaCGConfigKeyEnum.CKE_DEBUG_PRINT, Boolean.FALSE.toString());
        javaCGConfigureWrapper.setConfig(JavaCGConfigKeyEnum.CKE_OUTPUT_FILE_EXT, JACGConstants.EXT_MD);
        return javaCGConfigureWrapper;
    }

    private JACGUtil() {
        throw new IllegalStateException("illegal");
    }
}
