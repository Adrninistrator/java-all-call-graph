package com.adrninistrator.jacg.dto.wrapper;

/**
 * @author adrninistrator
 * @date 2022/8/25
 * @description: 完整方法及对应的方法HASH
 */
public class MethodAndHash {
    // 完整方法
    private String fullMethod;

    // 方法HASH
    private String methodHash;

    public String getFullMethod() {
        return fullMethod;
    }

    public void setFullMethod(String fullMethod) {
        this.fullMethod = fullMethod;
    }

    public String getMethodHash() {
        return methodHash;
    }

    public void setMethodHash(String methodHash) {
        this.methodHash = methodHash;
    }
}
