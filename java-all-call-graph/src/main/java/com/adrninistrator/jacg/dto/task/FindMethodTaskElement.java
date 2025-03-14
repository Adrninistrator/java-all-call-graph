package com.adrninistrator.jacg.dto.task;

/**
 * @author adrninistrator
 * @date 2023/6/18
 * @description: 用于查找方法的任务元素
 */
public class FindMethodTaskElement {

    // 完整方法HASH+长度
    private final String methodHash;

    // 完整方法信息
    private final String fullMethod;

    // 方法返回类型
    private final String returnType;

    public FindMethodTaskElement(String methodHash, String fullMethod, String returnType) {
        this.methodHash = methodHash;
        this.fullMethod = fullMethod;
        this.returnType = returnType;
    }

    public String getMethodHash() {
        return methodHash;
    }

    public String getFullMethod() {
        return fullMethod;
    }

    public String getReturnType() {
        return returnType;
    }

    @Override
    public String toString() {
        return fullMethod;
    }
}
