package com.adrninistrator.jacg.dto.task;

/**
 * @author adrninistrator
 * @date 2022/5/2
 * @description: 生成向上的方法调用链时，临时使用的方法信息
 */
public class CalleeTmpMethodInfo {
    // 完整方法HASH+长度
    private final String methodHash;

    // 完整方法信息
    private final String fullMethod;

    // 方法名+参数
    private final String methodNameAndArgs;

    public CalleeTmpMethodInfo(String methodHash, String fullMethod, String methodNameAndArgs) {
        this.methodHash = methodHash;
        this.fullMethod = fullMethod;
        this.methodNameAndArgs = methodNameAndArgs;
    }

    public String getMethodHash() {
        return methodHash;
    }

    public String getFullMethod() {
        return fullMethod;
    }

    public String getMethodNameAndArgs() {
        return methodNameAndArgs;
    }
}
