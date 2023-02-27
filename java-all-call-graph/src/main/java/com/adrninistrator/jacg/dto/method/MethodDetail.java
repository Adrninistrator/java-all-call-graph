package com.adrninistrator.jacg.dto.method;

import com.adrninistrator.jacg.util.JACGUtil;

/**
 * @author adrninistrator
 * @date 2022/12/10
 * @description: 方法详细信息
 */
public class MethodDetail {
    // 方法HASH+长度
    private final String methodHash;

    // 完整方法
    private final String fullMethod;

    // 完整类名
    private final String className;

    // 方法名
    private final String methodName;

    // 参数
    private final String argStr;

    // 参数数组
    private final String[] args;

    public MethodDetail(String fullMethod, String className, String methodName, String argStr, String[] args) {
        methodHash = JACGUtil.genHashWithLen(fullMethod);
        this.fullMethod = fullMethod;
        this.className = className;
        this.methodName = methodName;
        this.argStr = argStr;
        this.args = args;
    }

    public String getFullMethod() {
        return fullMethod;
    }

    public String getClassName() {
        return className;
    }

    public String getMethodName() {
        return methodName;
    }

    public String getArgStr() {
        return argStr;
    }

    public String[] getArgs() {
        return args;
    }

    @Override
    public String toString() {
        return fullMethod;
    }
}
