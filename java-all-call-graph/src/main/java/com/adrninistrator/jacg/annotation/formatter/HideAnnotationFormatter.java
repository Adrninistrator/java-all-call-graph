package com.adrninistrator.jacg.annotation.formatter;

import com.adrninistrator.jacg.dto.annotation.BaseAnnotationAttribute;

import java.util.Map;

/**
 * @author adrninistrator
 * @date 2023/2/16
 * @description: 不显示方法注解的处理类
 */
public class HideAnnotationFormatter extends AbstractAnnotationFormatter {
    @Override
    public boolean checkHandleAnnotation(String annotationName) {
        // 对所有的注解类型都处理
        return true;
    }

    @Override
    public String handleAnnotation(String fullMethod, String className, String annotationName, Map<String, BaseAnnotationAttribute> attributesMap) {
        // 返回null，代表不显示注解信息
        return null;
    }
}
