package com.adrninistrator.jacg.annotation.formatter;

import com.adrninistrator.jacg.common.JACGCommonNameConstants;
import com.adrninistrator.jacg.dto.annotation.BaseAnnotationAttribute;
import com.adrninistrator.jacg.dto.annotation.ListStringAnnotationAttribute;
import com.adrninistrator.javacg.common.JavaCGConstants;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Map;

/**
 * @author adrninistrator
 * @date 2022/4/8
 * @description: 处理方法上的Spring MVC RequestMapping注解处理类，返回@注解类名("path")
 */
public class SpringMvcRequestMappingFormatter extends AbstractAnnotationFormatter {
    private static final Logger logger = LoggerFactory.getLogger(SpringMvcRequestMappingFormatter.class);

    @Override
    public boolean checkHandleAnnotation(String annotationName) {
        return isRequestMappingAnnotation(annotationName);
    }

    @Override
    public String handleAnnotation(String fullMethod, String className, String annotationName, Map<String, BaseAnnotationAttribute> attributesMap) {
        String path = getPath(className, attributesMap);

        // 返回：注解类名(path)
        return annotationName + JavaCGConstants.FLAG_LEFT_BRACKET + path + JavaCGConstants.FLAG_RIGHT_BRACKET;
    }

    /**
     * 获取Spring MVC RequestMapping注解的path路径，与类上的属性进行拼接
     *
     * @param className     方法对应的完整类名
     * @param attributesMap 方法上的注解属性Map
     * @return path路径
     */
    public String getPath(String className, Map<String, BaseAnnotationAttribute> attributesMap) {
        StringBuilder path = new StringBuilder();

        // 获取Spring MVC对应类上的注解中的path
        String springMvcClassPath = getSpringMvcClassPath(className);
        if (springMvcClassPath != null) {
            if (!StringUtils.startsWith(springMvcClassPath, "/")) {
                path.append("/");
            }
            path.append(springMvcClassPath);
        }

        // 获取Spring MVC对应方法上的注解中的path
        String springMvcMethodPath = getPathInRequestMappingAnnotation(attributesMap);
        if (!StringUtils.startsWith(springMvcMethodPath, "/")) {
            path.append("/");
        }
        path.append(springMvcMethodPath);
        return path.toString();
    }

    // 判断是否为Spring MVC的RequestMapping注解
    private boolean isRequestMappingAnnotation(String annotationName) {
        return StringUtils.equalsAny(annotationName, JACGCommonNameConstants.SPRING_MVC_MAPPING_ANNOTATIONS);
    }

    // 获取Spring MVC对应注解中的path
    private String getPathInRequestMappingAnnotation(Map<String, BaseAnnotationAttribute> annotationAttributeMap) {
        for (String attributeName : JACGCommonNameConstants.SPRING_MVC_MAPPING_ATTRIBUTE_NAMES) {
            String path = doGetPathInRequestMappingAnnotation(annotationAttributeMap, attributeName);
            if (path != null) {
                return path;
            }
        }
        return "";
    }

    private String doGetPathInRequestMappingAnnotation(Map<String, BaseAnnotationAttribute> annotationAttributeMap, String attributeName) {
        ListStringAnnotationAttribute listStringAnnotationAttribute = annotationHandler.getAttributeFromMap(annotationAttributeMap, attributeName,
                ListStringAnnotationAttribute.class);
        if (listStringAnnotationAttribute == null) {
            // 尝试不同的属性名称，可能不存在，不需要打印日志
            return null;
        }

        return listStringAnnotationAttribute.getAttributeList().get(0);
    }

    // 获取Spring MVC对应类上的注解中的path
    private String getSpringMvcClassPath(String className) {
        Map<String, Map<String, BaseAnnotationAttribute>> classAnnotationMap = annotationHandler.queryAnnotationMap4Class(className);
        if (classAnnotationMap == null) {
            logger.error("未找到指定类的注解信息 {}", className);
            return null;
        }

        for (Map.Entry<String, Map<String, BaseAnnotationAttribute>> classAnnotationMapEntry : classAnnotationMap.entrySet()) {
            String annotationName = classAnnotationMapEntry.getKey();
            // 判断是否为Spring MVC的RequestMapping注解
            if (isRequestMappingAnnotation(annotationName)) {
                return getPathInRequestMappingAnnotation(classAnnotationMapEntry.getValue());
            }
        }
        return null;
    }

    /**
     * 从注解信息中获取Spring Mvc的path
     * 注解信息示例：  @org.springframework.web.bind.annotation.RequestMapping(/path1/path2.do)
     *
     * @param annotationInfo 注解信息
     * @return
     */
    public static String getSpringMvcPathFromAnnotation(String annotationInfo) {
        if (annotationInfo == null) {
            return null;
        }

        int indexLeftBracket = annotationInfo.indexOf(JavaCGConstants.FLAG_LEFT_BRACKET);
        if (indexLeftBracket == -1) {
            logger.error("注解信息中未找到{}字符 {}", JavaCGConstants.FLAG_LEFT_BRACKET, annotationInfo);
            return null;
        }

        int indexRightBracket = annotationInfo.lastIndexOf(JavaCGConstants.FLAG_RIGHT_BRACKET);
        if (indexRightBracket == -1) {
            logger.error("注解信息中未找到{}字符 {}", JavaCGConstants.FLAG_RIGHT_BRACKET, annotationInfo);
            return null;
        }

        if (indexLeftBracket >= indexRightBracket) {
            logger.error("注解信息中 {} {} 字符位置非法 {}", JavaCGConstants.FLAG_LEFT_BRACKET, JavaCGConstants.FLAG_RIGHT_BRACKET, annotationInfo);
            return null;
        }

        return annotationInfo.substring(indexLeftBracket + JavaCGConstants.FLAG_LEFT_BRACKET.length(), indexRightBracket);
    }
}
