package com.adrninistrator.jacg.extensions.annotation_handler;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.dto.annotation.AnnotationInfo4Read;

/**
 * @author adrninistrator
 * @date 2022/4/8
 * @description: 默认的方法注解处理类，在最后处理，对于每个注解都处理，返回@注解类名
 */
public class DefaultAnnotationHandler extends AbstractAnnotationHandler {
    @Override
    public boolean checkHandleAnnotation(String annotationName) {
        // 当前类最后一个执行，对所有的注解类型都处理
        return true;
    }

    @Override
    public String handleAnnotation(String fullMethod, String fullClassName, AnnotationInfo4Read annotationInfo4Read) {
        // 对注解的默认处理，返回@注解类名
        return JACGConstants.FLAG_AT + annotationInfo4Read.getAnnotationName();
    }
}
