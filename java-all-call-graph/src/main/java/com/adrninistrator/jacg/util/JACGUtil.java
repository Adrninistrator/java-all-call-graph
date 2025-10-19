package com.adrninistrator.jacg.util;

import com.adrninistrator.jacg.common.JACGCommonNameConstants;
import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.exceptions.JavaCG2RuntimeException;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.apache.commons.codec.digest.DigestUtils;
import org.apache.commons.lang3.ArrayUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Base64;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2021/6/17
 * @description:
 */

public class JACGUtil {
    private static final Logger logger = LoggerFactory.getLogger(JACGUtil.class);

    /**
     * 为字符串生成HASH+长度
     *
     * @param data
     * @return
     */
    public static String genHashWithLen(String data) {
        byte[] md5 = DigestUtils.md5(data);
        // 以下使用的BASE64方法输出结果范围为字母+"-"+"_"，不是原始的字母+"+"+"/"
        return String.format("%s#%03x", Base64.getUrlEncoder().encodeToString(md5), data.length());
    }

    /**
     * 从List中获取指定元素
     *
     * @param list
     * @param index
     * @param <E>
     * @return
     */
    public static <E> E getListElement(List<E> list, int index) {
        if (list == null || list.size() < index + 1) {
            return null;
        }
        return list.get(index);
    }

    /**
     * 从字符串找到第1个非指定字符的序号，并生成子字符串
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
     * 在字符串列表中查找字符串
     *
     * @param stringList 字符串列表
     * @param strings    需要判断的字符串
     * @return 在字符串列表中找到的字符串
     */
    public static String findStringInList(List<String> stringList, String... strings) {
        if (JavaCG2Util.isCollectionEmpty(stringList) || ArrayUtils.isEmpty(strings)) {
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
     * 获取变长参数中指定序号的参数，相关的参数都要使用包装类型，避免拆箱时产生空指针异常
     *
     * @param index
     * @param args
     * @param <T>
     * @return
     */
    @SuppressWarnings("unchecked")
    public static <T> T getArgAt(int index, Object... args) {
        if (ArrayUtils.isEmpty(args)) {
            logger.error("指定的参数为空");
            throw new JavaCG2RuntimeException("指定的参数为空");
        }
        if (index >= args.length) {
            logger.error("指定序号的参数不存在 {} {}", index, args.length);
            throw new JavaCG2RuntimeException("指定序号的参数不存在");
        }

        return (T) args[index];
    }

    /**
     * 获得对象的简单类名+对象HASH
     *
     * @param object
     * @return {对象的简单类名}@{对象HASH}
     */
    public static String getObjSimpleClassNameAndHash(Object object) {
        return object.getClass().getSimpleName() + JACGConstants.FLAG_AT + System.identityHashCode(object);
    }

    /**
     * 检查是否为Java基本类型包装类型，含String
     *
     * @param clazz
     * @return
     */
    public static boolean checkJavaBasicWrapperType(Class<?> clazz) {
        for (Class<?> tmpClass : JACGCommonNameConstants.JAVA_BASIC_WRAPPER_TYPES) {
            if (tmpClass == clazz) {
                return true;
            }
        }
        return false;
    }

    /**
     * 解析jar文件序号字符串
     *
     * @param jarNumStr
     * @return
     */
    public static Integer parseJarNum(String jarNumStr) {
        return JavaCG2Constants.EMPTY_JAR_NUM.equals(jarNumStr) ? null : Integer.parseInt(jarNumStr);
    }

    private JACGUtil() {
        throw new IllegalStateException("illegal");
    }
}
