package com.adrninistrator.jacg.dto.method;

/**
 * @author adrninistrator
 * @date 2022/8/29
 * @description: 调用链方法文件名中包含的信息
 */
public class MethodInfoInFileName {
    // 完整或简单类名
    private final String simpleClassName;

    // 方法名
    private final String methodName;

    // 方法HASH+长度
    private final String methodHash;

    public MethodInfoInFileName(String simpleClassName, String methodName, String methodHash) {
        this.simpleClassName = simpleClassName;
        this.methodName = methodName;
        this.methodHash = methodHash;
    }

    public String getSimpleClassName() {
        return simpleClassName;
    }

    public String getMethodName() {
        return methodName;
    }

    public String getMethodHash() {
        return methodHash;
    }
}
