package com.adrninistrator.jacg.dto.annotation;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2022/4/6
 * @description:
 */
public class MethodWithAnnotationInfo {
    // 完整方法名
    private String fullMethod;

    // 完整类名
    private String fullClassName;

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
}
