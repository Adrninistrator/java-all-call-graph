package com.adrninistrator.jacg.util;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.exceptions.JavaCG2RuntimeException;
import com.adrninistrator.javacg2.util.JavaCG2ClassMethodUtil;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.apache.commons.codec.digest.DigestUtils;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.Constructor;
import java.util.Base64;
import java.util.List;
import java.util.concurrent.ThreadPoolExecutor;

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
     * 等待指定时间
     *
     * @param time
     */
    public static void sleep(long time) {
        try {
            Thread.sleep(time);
        } catch (InterruptedException e) {
            logger.error("error ", e);
            Thread.currentThread().interrupt();
        }
    }

    /**
     * 根据类名获取对应实例，构造函数无参数
     *
     * @param className
     * @param classType
     * @param <T>
     * @return
     */
    @SuppressWarnings("unchecked")
    public static <T> T genClassObject(String className, Class<T> classType) {
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
     * 根据类名获取对应实例，构造函数有参数
     *
     * @param className
     * @param classType
     * @param argTypes
     * @param argValues
     * @param <T>
     * @return
     */
    @SuppressWarnings("unchecked")
    public static <T> T genClassObject(String className, Class<T> classType, Class<?>[] argTypes, Object[] argValues) {
        if (ArrayUtils.isEmpty(argTypes)) {
            logger.error("未指定参数类型");
            throw new JavaCG2RuntimeException("未指定参数类型");
        }
        if (ArrayUtils.isEmpty(argValues)) {
            logger.error("未指定参数值");
            throw new JavaCG2RuntimeException("未指定参数值");
        }
        try {
            Class<?> clazz = Class.forName(className);
            if (clazz.getSimpleName().startsWith("Abstract")) {
                logger.info("跳过抽象类 {}", clazz.getSimpleName());
                return null;
            }
            Constructor<?> constructor = clazz.getConstructor(argTypes);
            Object obj = constructor.newInstance(argValues);

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
     * 将源列表中的元素添加到目标列表中
     * 忽略空的元素，忽略重复的元素
     *
     * @param srcList
     * @param destList
     */
    public static void addList2List(List<String> srcList, List<String> destList) {
        if (srcList == null || destList == null) {
            throw new JavaCG2RuntimeException("参数不允许为空");
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
     * 获取变长参数中指定下标的参数，相关的参数都要使用包装类型，避免拆箱时产生空指针异常
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
            logger.error("指定下标的参数不存在 {} {}", index, args.length);
            throw new JavaCG2RuntimeException("指定下标的参数不存在");
        }

        return (T) args[index];
    }

    /**
     * 获取配置文件的根目录
     *
     * @return
     */
    public static String getInputRootPath() {
        return JavaCG2Util.getDirPathInJvmOptions(JavaCG2Constants.PROPERTY_INPUT_ROOT_PATH);
    }

    /**
     * 检查指定的字符串中是否包含回车换行
     *
     * @param data
     * @return
     */
    public static boolean containsCRLF(String data) {
        return StringUtils.containsAny(data, "\r", "\n");
    }

    /**
     * 获得对象的简单类名+对象HASH
     *
     * @param object
     * @return [对象的简单类名]@[对象HASH]
     */
    public static String getObjSimpleClassNameAndHash(Object object) {
        return object.getClass().getSimpleName() + JACGConstants.FLAG_AT + System.identityHashCode(object);
    }

    /**
     * 获取指定的调用堆栈中指定下标的方法
     *
     * @param stackTraceElements
     * @param index
     * @return
     */
    public static String getMethodInStackTrace(StackTraceElement[] stackTraceElements, int index) {
        if (ArrayUtils.isEmpty(stackTraceElements)) {
            return "";
        }

        if (stackTraceElements.length < index) {
            return "";
        }
        StackTraceElement stackTraceElement = stackTraceElements[index];
        return JavaCG2ClassMethodUtil.formatFullMethodWithArgTypes(stackTraceElement.getClassName(), stackTraceElement.getMethodName());
    }

    /**
     * 检查包名，若以.或/开头或结尾则检查不通过
     *
     * @param packageName
     * @return
     */
    public static boolean checkPackageName(String packageName) {
        return !StringUtils.startsWithAny(packageName, ".", "/") && !StringUtils.endsWithAny(packageName, ".", "/");
    }

    /**
     * 将包名替换为目录路径
     *
     * @param packageName
     */
    public static String replacePackage2DirPath(String packageName) {
        return packageName.replace('.', '/');
    }

    /**
     * 获得经过的时间秒数
     *
     * @param startTime
     * @return
     */
    public static double getSpendSeconds(long startTime) {
        return getSecondsFromMilli(System.currentTimeMillis() - startTime);
    }

    /**
     * 将耗时的毫秒数改为秒
     *
     * @param spendTime
     * @return
     */
    public static double getSecondsFromMilli(long spendTime) {
        return spendTime / 1000.0D;
    }

    private JACGUtil() {
        throw new IllegalStateException("illegal");
    }
}
