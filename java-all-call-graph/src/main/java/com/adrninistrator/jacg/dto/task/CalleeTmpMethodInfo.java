package com.adrninistrator.jacg.dto.task;

/**
 * @author adrninistrator
 * @date 2022/5/2
 * @description: 生成向上的方法调用链时，临时使用的方法信息
 */
public class CalleeTmpMethodInfo {
    // 完整方法HASH+长度
    private String methodHash;

    // 完整方法信息
    private String fullMethod;

    // 方法名+参数
    private String methodNameAndArgs;

    public String getMethodHash() {
        return methodHash;
    }

    public void setMethodHash(String methodHash) {
        this.methodHash = methodHash;
    }

    public String getFullMethod() {
        return fullMethod;
    }

    public void setFullMethod(String fullMethod) {
        this.fullMethod = fullMethod;
    }

    public String getMethodNameAndArgs() {
        return methodNameAndArgs;
    }

    public void setMethodNameAndArgs(String methodNameAndArgs) {
        this.methodNameAndArgs = methodNameAndArgs;
    }
}
