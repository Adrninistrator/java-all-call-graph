package com.adrninistrator.jacg.extensions.annotation_handler;

import com.adrninistrator.jacg.annotation.AnnotationStorage;
import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.dto.annotation.AnnotationInfo4Read;
import com.adrninistrator.jacg.util.JACGUtil;
import org.apache.commons.lang3.StringUtils;

import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2022/4/8
 * @description: 处理方法上的Spring MVC RequestMapping注解处理类，返回@注解类名("path")
 */
public class SpringMvcRequestMappingHandler extends AbstractAnnotationHandler {
    @Override
    public boolean checkHandleAnnotation(String annotationName) {
        return isRequestMappingAnnotation(annotationName);
    }

    @Override
    public String handleAnnotation(String fullMethod, String fullClassName, AnnotationInfo4Read annotationInfo4Read) {
        StringBuilder path = new StringBuilder();

        // 获取Spring MVC对应类上的注解中的path
        String springMvcClassPath = getSpringMvcClassPath(fullClassName);
        if (springMvcClassPath != null) {
            path.append("/").append(springMvcClassPath);
        }

        // 获取Spring MVC对应方法上的注解中的path
        String springMvcMethodPath = getPathInRequestMappingAnnotation(annotationInfo4Read.getAnnotationAttributeMap());
        path.append("/").append(springMvcMethodPath);

        // 返回@注解类名("path")
        return JACGConstants.FLAG_AT + annotationInfo4Read.getAnnotationName() + JACGConstants.FLAG_LEFT_BRACKET + "\"" + path + "\"" + JACGConstants.FLAG_RIGHT_BRACKET;
    }

    // 判断是否为Spring MVC的RequestMapping注解
    private boolean isRequestMappingAnnotation(String annotationName) {
        return StringUtils.equalsAny(annotationName,
                "org.springframework.web.bind.annotation.RequestMapping",
                "org.springframework.web.bind.annotation.PatchMapping",
                "org.springframework.web.bind.annotation.DeleteMapping",
                "org.springframework.web.bind.annotation.PutMapping",
                "org.springframework.web.bind.annotation.PostMapping",
                "org.springframework.web.bind.annotation.GetMapping"
        );
    }

    // 获取Spring MVC对应注解中的path
    private String getPathInRequestMappingAnnotation(Map<String, String> annotationAttributeMap) {
        String path = annotationAttributeMap.get("path");
        if (path != null) {
            return JACGUtil.getAnnotationArrayAttributeValue(path);
        }
        path = annotationAttributeMap.get("value");
        if (path != null) {
            return JACGUtil.getAnnotationArrayAttributeValue(path);
        }
        return "";
    }

    // 获取Spring MVC对应类上的注解中的path
    private String getSpringMvcClassPath(String fullClassName) {
        List<AnnotationInfo4Read> annotationInfo4ReadList = AnnotationStorage.getAnnotationInfo4Class(fullClassName);
        if (annotationInfo4ReadList == null) {
            return null;
        }

        for (AnnotationInfo4Read annotationInfo4Read : annotationInfo4ReadList) {
            if (isRequestMappingAnnotation(annotationInfo4Read.getAnnotationName())) {
                return getPathInRequestMappingAnnotation(annotationInfo4Read.getAnnotationAttributeMap());
            }
        }
        return null;
    }
}
