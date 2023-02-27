package com.adrninistrator.jacg.util.spring;

import com.adrninistrator.jacg.common.JACGCommonNameConstants;
import org.apache.commons.lang3.StringUtils;

/**
 * @author adrninistrator
 * @date 2022/11/17
 * @description: Spring MVC相关的RequestMapping等注解处理
 */
public class SpringMvcRequestMappingUtil {
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
     * 生成用于显示的URI
     * 格式为"/classPath/methodPath"
     *
     * @param classPath
     * @param methodPath
     * @return
     */
    public static String genShowUri(String classPath, String methodPath) {
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
        return stringBuilder.toString();
    }

    private SpringMvcRequestMappingUtil() {
        throw new IllegalStateException("illegal");
    }
}
