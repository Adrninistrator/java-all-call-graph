package com.adrninistrator.jacg.dto.annotation;

/**
 * @author adrninistrator
 * @date 2022/4/6
 * @description:
 */
public class MethodWithAnnotationInfo {
    // 完整方法名
    private final String fullMethod;

    // 完整类名
    private final String fullClassName;

    public MethodWithAnnotationInfo(String fullMethod, String fullClassName) {
        this.fullMethod = fullMethod;
        this.fullClassName = fullClassName;
    }

    public String getFullMethod() {
        return fullMethod;
    }

    public String getFullClassName() {
        return fullClassName;
    }
}
