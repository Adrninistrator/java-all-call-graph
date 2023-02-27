package com.adrninistrator.jacg.dto.task;

/**
 * @author adrninistrator
 * @date 2022/5/2
 * @description: 生成向上的方法调用链时，查找入口方法时使用的信息
 */
public class CalleeEntryMethodTaskInfo {
    // 完整方法HASH+长度
    private final String methodHash;

    // 完整方法信息
    private final String fullMethod;

    // 方法名+参数
    private final String methodNameAndArgs;

    // 方法调用的标志
    private final int callFlags;

    public CalleeEntryMethodTaskInfo(String methodHash, String fullMethod, String methodNameAndArgs, int callFlags) {
        this.methodHash = methodHash;
        this.fullMethod = fullMethod;
        this.methodNameAndArgs = methodNameAndArgs;
        this.callFlags = callFlags;
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

    public int getCallFlags() {
        return callFlags;
    }
}
