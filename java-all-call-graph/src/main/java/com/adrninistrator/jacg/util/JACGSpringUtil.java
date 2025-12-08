package com.adrninistrator.jacg.util;

import com.adrninistrator.jacg.common.JACGCommonNameConstants;
import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.exceptions.JavaCG2RuntimeException;
import com.adrninistrator.javacg2.util.JavaCG2ClassMethodUtil;
import org.apache.commons.lang3.StringUtils;
import org.aspectj.weaver.AdviceKind;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * @author adrninistrator
 * @date 2022/11/17
 * @description: Spring相关工具类
 */
public class JACGSpringUtil {

    private static final AdviceKind[] ADVICE_KINDS = new AdviceKind[]{
            AdviceKind.Before,
            AdviceKind.After,
            AdviceKind.Around,
            AdviceKind.AfterReturning,
            AdviceKind.AfterThrowing
    };

    public static final String AOP_XML_PREFIX = "aop:";

    public static final Pattern PATTERN_CLASS_NAME = Pattern.compile("\\b([a-zA-Z_]\\w*\\.)+[a-zA-Z_]\\w*\\b");

    /**
     * 判断是否为Spring MVC的Controller注解
     *
     * @param annotationName
     * @return
     */
    public static boolean isControllerAnnotation(String annotationName) {
        return StringUtils.equalsAny(annotationName, JACGCommonNameConstants.SPRING_MVC_CONTROLLER_ANNOTATIONS);
    }

    /**
     * 判断是否为Spring MVC的RequestMapping注解
     *
     * @param annotationName
     * @return
     */
    public static boolean isRequestMappingAnnotation(String annotationName) {
        return StringUtils.equalsAny(annotationName, JACGCommonNameConstants.SPRING_MVC_MAPPING_ANNOTATIONS);
    }

    /**
     * 判断是否为Spring MVC的RequestMapping注解的path属性
     *
     * @param attributeName
     * @return
     */
    public static boolean isRequestMappingPathAttribute(String attributeName) {
        return StringUtils.equalsAny(attributeName, JACGCommonNameConstants.SPRING_MVC_MAPPING_ATTRIBUTE_NAMES);
    }

    /**
     * 判断是否为Spring MVC的RequestMapping注解
     *
     * @param annotationName
     * @return
     */
    public static boolean isTaskAnnotation(String annotationName) {
        return StringUtils.equals(annotationName, JACGCommonNameConstants.SPRING_TASK_ANNOTATION);
    }

    /**
     * 生成Spring Controller用于显示的URI
     * 格式为"/classPath/methodPath"
     *
     * @param classPath
     * @param methodPath
     * @return
     */
    public static String genSpringControllerShowUri(String classPath, String methodPath) {
        StringBuilder stringBuilder = new StringBuilder();
        if (StringUtils.isNotBlank(classPath)) {
            if (!classPath.startsWith("/")) {
                stringBuilder.append("/");
            }
            stringBuilder.append(classPath);
        }

        if (StringUtils.isNotBlank(methodPath)) {
            if (!methodPath.startsWith("/")) {
                stringBuilder.append("/");
            }
            stringBuilder.append(methodPath);
        }
        String uri = stringBuilder.toString();
        // 去掉重复的/
        while (uri.contains("//")) {
            uri = StringUtils.replace(uri, "//", "/");
        }
        // 去掉最后的/
        if (uri.endsWith("/")) {
            uri = uri.substring(0, uri.length() - "/".length());
        }
        return uri;
    }

    /**
     * 根据注解简单类名获得advice类型
     *
     * @param annotationSimpleClassName
     * @return
     */
    public static AdviceKind getAdviceKindFromAnnotationSCN(String annotationSimpleClassName) {
        for (AdviceKind adviceKind : ADVICE_KINDS) {
            if (StringUtils.equalsIgnoreCase(adviceKind.getName(), annotationSimpleClassName)) {
                return adviceKind;
            }
        }
        throw new JavaCG2RuntimeException("非法的advice注解简单类名 " + annotationSimpleClassName);
    }

    /**
     * 根据XML元素名称获得advice类型
     *
     * @param xmlElementName
     * @return
     */
    public static AdviceKind getAdviceKindFromXmlElement(String xmlElementName) {
        String xmlElementSuffix = StringUtils.replace(xmlElementName.substring(AOP_XML_PREFIX.length()), "-", "");
        return getAdviceKindFromAnnotationSCN(xmlElementSuffix);
    }

    /**
     * 解析Spring AOP表达式中的类名，不返回JDK中的类
     *
     * @param expression
     * @return
     */
    public static Set<String> parseClassNameInAopExpression(String expression) {
        Set<String> classNameSet = new HashSet<>();
        Matcher classNameMatcher = PATTERN_CLASS_NAME.matcher(expression);
        while (classNameMatcher.find()) {
            String className = classNameMatcher.group();
            if (JavaCG2ClassMethodUtil.isClassInJdk(className)) {
                // 跳过JDK中的类
                continue;
            }
            classNameSet.add(className);
            int dotCount = StringUtils.countMatches(expression, JavaCG2Constants.FLAG_DOT);
            if (dotCount >= 2) {
                // 假如找到的字符串中出现的.大于等于2次，则将开头到最后的.的内容也记录下来，有可能是“包名.类名.方法名”的形式
                classNameSet.add(StringUtils.substringBeforeLast(expression, JavaCG2Constants.FLAG_DOT));
            }
        }
        return classNameSet;
    }

    /**
     * 根据Spring Bean名称获得Spring Bean类名
     *
     * @param springBeanMap  记录Spring Bean相关信息的Map
     * @param springBeanName Spring Bean名称
     * @return
     */
    public static String getSpringBeanClassName(Map<String, String> springBeanMap, String springBeanName) {
        String springBeanClassName = springBeanMap.get(springBeanName);
        if (springBeanClassName == null) {
            // 假如根据Spring Bean名称未获取到对应的类名，则将首字段修改为小写后继续尝试获取
            String springBeanNameLower = JavaCG2ClassMethodUtil.getFirstLetterLowerClassName(springBeanName);
            springBeanClassName = springBeanMap.get(springBeanNameLower);
        }
        return springBeanClassName;
    }

    private JACGSpringUtil() {
        throw new IllegalStateException("illegal");
    }
}
