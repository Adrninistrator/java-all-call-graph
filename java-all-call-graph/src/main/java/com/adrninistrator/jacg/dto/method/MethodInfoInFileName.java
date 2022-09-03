package com.adrninistrator.jacg.dto.method;

/**
 * @author adrninistrator
 * @date 2022/8/29
 * @description: 调用链方法文件名中包含的信息
 */
public class MethodInfoInFileName {

    // 完整或简单类名
    private String simpleClassName;

    // 方法名
    private String methodName;

    // 方法HASH+长度
    private String methodHash;

    public String getSimpleClassName() {
        return simpleClassName;
    }

    public void setSimpleClassName(String simpleClassName) {
        this.simpleClassName = simpleClassName;
    }

    public String getMethodName() {
        return methodName;
    }

    public void setMethodName(String methodName) {
        this.methodName = methodName;
    }

    public String getMethodHash() {
        return methodHash;
    }

    public void setMethodHash(String methodHash) {
        this.methodHash = methodHash;
    }
}
