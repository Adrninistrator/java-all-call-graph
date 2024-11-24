package com.adrninistrator.jacg.dto.method;

/**
 * @author adrninistrator
 * @date 2022/8/25
 * @description: 完整方法及对应的方法HASH
 */
public class MethodAndHash {
    // 完整方法
    private final String fullMethod;

    // 方法HASH
    private final String methodHash;

    public MethodAndHash(String fullMethod, String methodHash) {
        this.fullMethod = fullMethod;
        this.methodHash = methodHash;
    }

    @Override
    public String toString() {
        return "MethodAndHash{" +
                "fullMethod='" + fullMethod + '\'' +
                ", methodHash='" + methodHash + '\'' +
                '}';
    }

    public String getFullMethod() {
        return fullMethod;
    }

    public String getMethodHash() {
        return methodHash;
    }
}
