package com.adrninistrator.jacg.dto.lambda;

/**
 * @author adrninistrator
 * @date 2023/1/10
 * @description: Lambda表达式方法调用
 */
public class LambdaMethodCall {
    // 方法调用call_id
    private int callId;

    // 调用方完整方法
    private String callerFullMethod;

    // 调用方代码行号
    private int callerLineNumber;

    // 被调用方完整方法（Lambda表达式中实际被调用的方法）
    private String calleeFullMethod;

    // 被调用的Lambda表达式完整方法
    private String lambdaCalleeFullMethod;

    /*
        以下字段可能为null
     */
    // Lambda表达式下一个被调用完整方法
    private String lambdaNextFullMethod;

    // 下一个被调用方法是否为Stream
    private Boolean lambdaNextIsStream;

    // 下一个被调用方法是否为Stream的intermediate（中间）操作
    private Boolean lambdaNextIsIntermediate;

    // 下一个被调用方法是否为Stream的terminal（终端）操作
    private Boolean lambdaNextIsTerminal;

    public int getCallId() {
        return callId;
    }

    public void setCallId(int callId) {
        this.callId = callId;
    }

    public String getCallerFullMethod() {
        return callerFullMethod;
    }

    public void setCallerFullMethod(String callerFullMethod) {
        this.callerFullMethod = callerFullMethod;
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

    public String getLambdaCalleeFullMethod() {
        return lambdaCalleeFullMethod;
    }

    public void setLambdaCalleeFullMethod(String lambdaCalleeFullMethod) {
        this.lambdaCalleeFullMethod = lambdaCalleeFullMethod;
    }

    public String getLambdaNextFullMethod() {
        return lambdaNextFullMethod;
    }

    public void setLambdaNextFullMethod(String lambdaNextFullMethod) {
        this.lambdaNextFullMethod = lambdaNextFullMethod;
    }

    public Boolean getLambdaNextIsStream() {
        return lambdaNextIsStream;
    }

    public void setLambdaNextIsStream(Boolean lambdaNextIsStream) {
        this.lambdaNextIsStream = lambdaNextIsStream;
    }

    public Boolean getLambdaNextIsIntermediate() {
        return lambdaNextIsIntermediate;
    }

    public void setLambdaNextIsIntermediate(Boolean lambdaNextIsIntermediate) {
        this.lambdaNextIsIntermediate = lambdaNextIsIntermediate;
    }

    public Boolean getLambdaNextIsTerminal() {
        return lambdaNextIsTerminal;
    }

    public void setLambdaNextIsTerminal(Boolean lambdaNextIsTerminal) {
        this.lambdaNextIsTerminal = lambdaNextIsTerminal;
    }

    @Override
    public String toString() {
        return calleeFullMethod;
    }
}
