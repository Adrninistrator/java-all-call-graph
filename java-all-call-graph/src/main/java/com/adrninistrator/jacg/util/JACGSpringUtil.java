package com.adrninistrator.jacg.util;

import com.adrninistrator.jacg.common.JACGCommonNameConstants;
import org.apache.commons.lang3.StringUtils;

/**
 * @author adrninistrator
 * @date 2022/11/17
 * @description: Spring相关工具类
 */
public class JACGSpringUtil {
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

    private JACGSpringUtil() {
        throw new IllegalStateException("illegal");
    }
}
