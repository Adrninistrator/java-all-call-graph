package com.adrninistrator.jacg.dto.method;

/**
 * @author adrninistrator
 * @date 2022/8/25
 * @description: 完整方法与返回类型及对应的方法HASH
 */
public class MethodAndHash {
    // 完整方法
    private final String fullMethod;

    // 方法返回类型
    private final String methodReturnType;

    // 方法HASH
    private final String methodHash;

    public MethodAndHash(String fullMethod, String methodReturnType, String methodHash) {
        this.fullMethod = fullMethod;
        this.methodReturnType = methodReturnType;
        this.methodHash = methodHash;
    }

    @Override
    public String toString() {
        return "MethodAndHash{" +
                "fullMethod='" + fullMethod + '\'' +
                "methodReturnType='" + methodReturnType + '\'' +
                ", methodHash='" + methodHash + '\'' +
                '}';
    }

    public String getFullMethod() {
        return fullMethod;
    }

    public String getMethodReturnType() {
        return methodReturnType;
    }

    public String getMethodHash() {
        return methodHash;
    }
}
