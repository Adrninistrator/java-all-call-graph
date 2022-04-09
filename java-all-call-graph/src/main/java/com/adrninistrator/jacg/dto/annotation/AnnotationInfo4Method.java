package com.adrninistrator.jacg.dto.annotation;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2022/4/6
 * @description:
 */
public class AnnotationInfo4Method {
    // 完整方法名
    private String fullMethod;

    // 完整类名
    private String fullClassName;

    private List<AnnotationInfo4Read> annotationInfo4ReadList;

    public String getFullMethod() {
        return fullMethod;
    }

    public void setFullMethod(String fullMethod) {
        this.fullMethod = fullMethod;
    }

    public String getFullClassName() {
        return fullClassName;
    }

    public void setFullClassName(String fullClassName) {
        this.fullClassName = fullClassName;
    }

    public List<AnnotationInfo4Read> getAnnotationInfo4ReadList() {
        return annotationInfo4ReadList;
    }

    public void setAnnotationInfo4ReadList(List<AnnotationInfo4Read> annotationInfo4ReadList) {
        this.annotationInfo4ReadList = annotationInfo4ReadList;
    }
}
