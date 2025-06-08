package com.adrninistrator.jacg.jardiff.dto.method;

/**
 * @author adrninistrator
 * @date 2024/3/13
 * @description: 发生变化的方法信息
 */
public class ModifiedMethodInfo {

    // 完整方法
    private final String fullMethod;

    // 方法返回类型
    private final String methodReturnType;

    // 是否存在旧方法，true: 存在旧方法 false: 不存在旧方法（是新方法）
    private final boolean oldMethodExists;

    public ModifiedMethodInfo(String fullMethod, String methodReturnType, boolean oldMethodExists) {
        this.fullMethod = fullMethod;
        this.methodReturnType = methodReturnType;
        this.oldMethodExists = oldMethodExists;
    }

    public String getFullMethod() {
        return fullMethod;
    }

    public String getMethodReturnType() {
        return methodReturnType;
    }

    public boolean isOldMethodExists() {
        return oldMethodExists;
    }

    @Override
    public String toString() {
        return fullMethod;
    }
}
