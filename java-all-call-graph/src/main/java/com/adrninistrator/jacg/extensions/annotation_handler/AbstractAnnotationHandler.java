package com.adrninistrator.jacg.extensions.annotation_handler;

import com.adrninistrator.jacg.dto.annotation.AnnotationInfo4Read;

/**
 * @author adrninistrator
 * @date 2022/4/7
 * @description: 方法注解处理类的基类
 */
public abstract class AbstractAnnotationHandler {

    /**
     * 判断当前类是否处理对应的注解
     * 在项目中可以指定多个方法注解处理类，当某个类的该方法返回true时，调用handleAnnotation获取对应的注解信息，不再调用后续的方法注解处理类进行处理
     *
     * @param annotationName
     * @return true: 当前类处理对应的注解 false: 当前类不处理对应的注解
     */
    public abstract boolean checkHandleAnnotation(String annotationName);

    /**
     * 返回方法上的注解处理后的结果
     *
     * @param fullMethod
     * @param fullClassName
     * @param annotationInfo4Read
     * @return
     */
    public abstract String handleAnnotation(String fullMethod, String fullClassName, AnnotationInfo4Read annotationInfo4Read);
}
