package com.adrninistrator.jacg.dto.method;

/**
 * @author adrninistrator
 * @date 2022/12/10
 * @description: 方法详细信息
 */
public class MethodDetail extends ClassAndMethodName {
    // 完整方法
    private final String fullMethod;

    // 参数
    private final String argStr;

    // 参数数组
    private final String[] args;

    public MethodDetail(String fullMethod, String className, String methodName, String argStr, String[] args) {
        super(className, methodName);
        this.fullMethod = fullMethod;
        this.argStr = argStr;
        this.args = args;
    }

    public String getFullMethod() {
        return fullMethod;
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
