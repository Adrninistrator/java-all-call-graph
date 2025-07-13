package com.adrninistrator.jacg.util;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.dto.method.ClassAndMethodName;
import com.adrninistrator.jacg.dto.method.FullMethodWithReturnType;
import com.adrninistrator.jacg.dto.method.MethodDetail;
import com.adrninistrator.jacg.dto.method.MethodDetailNoReturnType;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCall;
import com.adrninistrator.jacg.handler.common.enums.ClassInterfaceEnum;
import com.adrninistrator.javacg2.common.JavaCG2CommonNameConstants;
import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.common.enums.JavaCG2CallTypeEnum;
import com.adrninistrator.javacg2.exceptions.JavaCG2Error;
import com.adrninistrator.javacg2.exceptions.JavaCG2RuntimeException;
import com.adrninistrator.javacg2.util.JavaCG2ByteCodeUtil;
import com.adrninistrator.javacg2.util.JavaCG2ClassMethodUtil;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2022/11/19
 * @description: 类名与方法处理工具类
 */
public class JACGClassMethodUtil {
    private static final Logger logger = LoggerFactory.getLogger(JACGClassMethodUtil.class);

    /**
     * 生成方法HASH+长度
     *
     * @param fullMethod 完整方法
     * @param returnType 返回类型（包含数组标志）
     * @return
     */
    public static String genMethodHashWithLen(String fullMethod, String returnType) {
        if (StringUtils.contains(fullMethod, JACGConstants.CLASS_PLACE_HOLDER)
                && StringUtils.contains(fullMethod, JACGConstants.METHOD_PLACE_HOLDER)) {
            // 当方法使用占位符时，允许返回类型为空
            return "";
        }

        if (StringUtils.isBlank(returnType)) {
            throw new JavaCG2Error("返回类型为空 " + fullMethod);
        }
        return JACGUtil.genHashWithLen(JavaCG2ClassMethodUtil.genFullMethodWithReturnType(fullMethod, returnType));
    }

    /**
     * 将完整方法与返回类型列表生成字符串列表
     *
     * @param fullMethodWithReturnTypeList
     * @return
     */
    public static List<String> genFullMethodWithReturnTypeStrList(List<FullMethodWithReturnType> fullMethodWithReturnTypeList) {
        List<String> list = new ArrayList<>(fullMethodWithReturnTypeList.size());
        for (FullMethodWithReturnType fullMethodWithReturnType : fullMethodWithReturnTypeList) {
            list.add(fullMethodWithReturnType.genFullMethodWithReturnType());
        }
        Collections.sort(list);
        return list;
    }

    /**
     * 将完整方法与返回类型列表生成字符串Set
     *
     * @param fullMethodWithReturnTypeSet
     * @return
     */
    public static Set<String> genFullMethodWithReturnTypeStrSet(Set<FullMethodWithReturnType> fullMethodWithReturnTypeSet) {
        Set<String> list = new HashSet<>(fullMethodWithReturnTypeSet.size());
        for (FullMethodWithReturnType fullMethodWithReturnType : fullMethodWithReturnTypeSet) {
            list.add(fullMethodWithReturnType.genFullMethodWithReturnType());
        }
        return list;
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
    public static String genSafeMethodName(String methodName) {
        if (methodName == null) {
            return null;
        }
        return methodName.replace('<', '(')
                .replace('>', ')')
                .replace(':', '@');
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
     * 根据完整方法生成方法详细信息，不包含返回类型
     *
     * @param fullMethod
     * @return
     */
    public static MethodDetailNoReturnType genMethodDetailNoReturnType(String fullMethod) {
        MethodDetailNoReturnType methodDetailNoReturnType = new MethodDetailNoReturnType();
        genMethodDetailNoReturnType(fullMethod, methodDetailNoReturnType);
        return methodDetailNoReturnType;
    }

    /**
     * 根据完整方法+返回类型生成方法详细信息
     *
     * @param fullMethodWithReturnType
     * @return
     */
    public static MethodDetail genMethodDetail(String fullMethodWithReturnType) {
        String fullMethod = StringUtils.substringBeforeLast(fullMethodWithReturnType, JavaCG2Constants.FLAG_COLON);
        String returnType = StringUtils.substringAfterLast(fullMethodWithReturnType, JavaCG2Constants.FLAG_COLON);
        MethodDetail methodDetail = new MethodDetail();
        genMethodDetailNoReturnType(fullMethod, methodDetail);
        methodDetail.setReturnType(returnType);
        return methodDetail;
    }

    /**
     * 根据完整方法与返回类型生成方法详细信息
     *
     * @param fullMethod
     * @param returnType
     * @return
     */
    public static MethodDetail genMethodDetail(String fullMethod, String returnType) {
        MethodDetail methodDetail = new MethodDetail();
        genMethodDetailNoReturnType(fullMethod, methodDetail);
        methodDetail.setReturnType(returnType);
        return methodDetail;
    }

    /**
     * 根据完整方法生成方法详细信息，不包含返回类型
     *
     * @param fullMethod
     * @param methodDetailNoReturnType
     * @return
     */
    public static void genMethodDetailNoReturnType(String fullMethod, MethodDetailNoReturnType methodDetailNoReturnType) {
        String className = JavaCG2ClassMethodUtil.getClassNameFromMethod(fullMethod);
        int indexColon = fullMethod.indexOf(JavaCG2Constants.FLAG_COLON);
        int indexLeftBrackets = fullMethod.indexOf(JavaCG2Constants.FLAG_LEFT_BRACKET);
        String methodName = fullMethod.substring(indexColon + 1, indexLeftBrackets);
        int indexRightBrackets = fullMethod.lastIndexOf(JavaCG2Constants.FLAG_RIGHT_BRACKET);
        // 不包含括号的方法参数类型字符串
        String argTypeStr = fullMethod.substring(indexLeftBrackets + JavaCG2Constants.FLAG_LEFT_BRACKET.length(), indexRightBrackets);
        methodDetailNoReturnType.setFullMethod(fullMethod);
        methodDetailNoReturnType.setClassName(className);
        methodDetailNoReturnType.setMethodName(methodName);
        methodDetailNoReturnType.setArgTypeStr(argTypeStr);
        methodDetailNoReturnType.setArgTypeList(genMethodArgTypeList(fullMethod));
    }

    /**
     * 根据完整方法生成方法参数类型列表
     *
     * @param fullMethod
     * @return
     */
    public static List<String> genMethodArgTypeList(String fullMethod) {
        String[] argTypes = StringUtils.splitPreserveAllTokens(JavaCG2ClassMethodUtil.getMethodArgTypes(fullMethod), JavaCG2Constants.FLAG_COMMA);
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
     * 根据{类名}:{方法名}格式的字符串获取对应的类名与方法名对象
     *
     * @param methodInfo
     * @return
     */
    public static ClassAndMethodName parseClassAndMethodName(String methodInfo) {
        String[] array = StringUtils.splitPreserveAllTokens(methodInfo, JavaCG2Constants.FLAG_COLON);
        if (array == null || array.length != 2) {
            throw new JavaCG2RuntimeException("指定的字符串不满足{类名}:{方法名}格式 " + methodInfo);
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
     *                               格式：{类名}:{方法名}
     *                               示例：org.slf4j.Logger:error org.slf4j.Logger:info org.slf4j.Logger:warn
     * @return
     */
    public static List<ClassAndMethodName> genClassAndMethodNameListFromString(String[] classMethodStringArray) {
        List<ClassAndMethodName> classAndMethodNameList = new ArrayList<>(classMethodStringArray.length);
        for (String expectedMethod : classMethodStringArray) {
            classAndMethodNameList.add(parseClassAndMethodName(expectedMethod));
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
        String className = JavaCG2ClassMethodUtil.getClassNameFromMethod(fullMethod);
        String methodName = JavaCG2ClassMethodUtil.getMethodNameFromFull(fullMethod);
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

    /**
     * 获取指定的调用堆栈中指定序号的方法
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
                logger.error("指定的类 {} 不是 {} 的子类或实现类", className, classType.getName());
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
                logger.error("指定的类 {} 不是 {} 的子类或实现类", className, classType.getName());
                return null;
            }

            return (T) obj;
        } catch (Exception e) {
            logger.error("根据指定类名 {} 获得 {} 类的实例异常 ", className, classType.getName(), e);
            return null;
        }
    }

    /**
     * 获得类中非静态字段的名称，包含父类中的字段
     *
     * @param clazz
     * @return
     */
    public static Set<String> getNonStaticFieldNameSet(Class<?> clazz) {
        Set<String> fieldNameSet = new HashSet<>();
        Class<?> currentClass = clazz;

        while (currentClass != null && !JavaCG2ClassMethodUtil.isClassInJdk(currentClass.getName())) {
            Field[] fields = currentClass.getDeclaredFields();
            for (Field field : fields) {
                if (!Modifier.isStatic(field.getModifiers())) {
                    fieldNameSet.add(field.getName());
                }
            }
            currentClass = currentClass.getSuperclass();
        }
        return fieldNameSet;
    }

    /**
     * 获得类中值非空的非静态字段的名称，包含父类中的字段
     *
     * @param object
     * @return
     */
    public static Set<String> getNonStaticNotNullFieldNameSet(Object object) {
        Set<String> fieldNameSet = new HashSet<>();
        Class<?> clazz = object.getClass();

        while (clazz != null && !JavaCG2ClassMethodUtil.isClassInJdk(clazz.getName())) {
            Field[] fields = clazz.getDeclaredFields();
            for (Field field : fields) {
                if (Modifier.isStatic(field.getModifiers())) {
                    continue;
                }

                try {
                    field.setAccessible(true);
                    Object value = field.get(object);
                    if (value != null) {
                        fieldNameSet.add(field.getName());
                    }
                } catch (IllegalAccessException e) {
                    logger.warn("获取字段值失败 {} {}", clazz.getName(), field.getName());
                }
            }
            clazz = clazz.getSuperclass();
        }

        return fieldNameSet;
    }

    /**
     * 根据Java的Method对象获得完整方法
     *
     * @param className
     * @param method
     * @return
     */
    public static String genJavaFullMethod(String className, Method method) {
        return JavaCG2ClassMethodUtil.formatFullMethod(className, method.getName(), method.getParameterTypes());
    }

    private JACGClassMethodUtil() {
        throw new IllegalStateException("illegal");
    }
}
