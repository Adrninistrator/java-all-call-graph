package com.adrninistrator.jacg.handler.dto.exception;

/**
 * @author adrninistrator
 * @date 2024/1/9
 * @description: catch的异常对象未被使用时，catch代码块中被调用方法不符合预期
 */
public class NoMCEU4MethodCall extends BaseMethodCatchExceptionUsage {

    // 方法调用的代码行号
    private int callerLineNumber;

    // 被调用完整方法
    private String calleeFullMethod;

    @Override
    public String getUsageDescription() {
        if (!isUseEInExpectedMethodCall()) {
            return "catch的异常对象未被使用时，catch代码块中被调用方法不符合预期";
        }
        return "catch的异常对象未被使用时，catch代码块中被调用方法符合预期";
    }

    @Override
    public String getUsageDetail() {
        return "代码第 " + callerLineNumber + " 行调用了方法 " + calleeFullMethod;
    }

    public int getCallerLineNumber() {
        return callerLineNumber;
    }

    public void setCallerLineNumber(int callerLineNumber) {
        this.callerLineNumber = callerLineNumber;
    }

    public String getCalleeFullMethod() {
        return calleeFullMethod;
    }

    public void setCalleeFullMethod(String calleeFullMethod) {
        this.calleeFullMethod = calleeFullMethod;
    }
}
