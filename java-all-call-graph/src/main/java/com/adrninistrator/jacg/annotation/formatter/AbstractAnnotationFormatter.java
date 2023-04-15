package com.adrninistrator.jacg.annotation.formatter;

import com.adrninistrator.jacg.dto.annotation.BaseAnnotationAttribute;
import com.adrninistrator.jacg.handler.annotation.AnnotationHandler;

import java.util.Map;

/**
 * @author adrninistrator
 * @date 2022/4/7
 * @description: 方法注解处理类的基类
 */
public abstract class AbstractAnnotationFormatter {

    // 注解相关的查询处理类
    protected AnnotationHandler annotationHandler;

    /**
     * 判断当前类是否处理对应的注解
     * 在项目中可以指定多个方法注解处理类，当某个类的该方法返回true时，调用handleAnnotation获取对应的注解信息，不再调用后续的方法注解处理类进行处理
     *
     * @param annotationName 注解类名
     * @return true: 当前类处理对应的注解 false: 当前类不处理对应的注解
     */
    public abstract boolean checkHandleAnnotation(String annotationName);

    /**
     * 返回方法上的注解处理后的结果
     * 当前方法不需要以@开头
     *
     * @param fullMethod     完整方法
     * @param className      方法所在类完整类名
     * @param annotationName 注解类名
     * @param attributesMap  注解属性Map
     * @return
     */
    public abstract String handleAnnotation(String fullMethod, String className, String annotationName, Map<String, BaseAnnotationAttribute> attributesMap);

    public void setAnnotationHandler(AnnotationHandler annotationHandler) {
        this.annotationHandler = annotationHandler;
    }
}
