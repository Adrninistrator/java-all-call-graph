package com.adrninistrator.jacg.dto.lambda;

import com.adrninistrator.jacg.dto.method.MethodDetail;

/**
 * @author adrninistrator
 * @date 2023/2/14
 * @description: Lambda表达式方法调用，包含各方法的详细信息
 */
public class LambdaMethodCallDetail extends LambdaMethodCall {
    // 调用方完整方法
    private MethodDetail callerFullMethodDetail;

    // 被调用方完整方法（Lambda表达式中实际被调用的方法）
    private MethodDetail calleeFullMethodDetail;

    // 被调用的Lambda表达式完整方法
    private MethodDetail lambdaCalleeFullMethodDetail;

    /*
        以下字段可能为null
     */
    // Lambda表达式下一个被调用完整方法
    private MethodDetail lambdaNextFullMethodDetail;

    public static LambdaMethodCallDetail genLambdaMethodCallDetail(LambdaMethodCall lambdaMethodCall) {
        LambdaMethodCallDetail lambdaMethodCallDetail = new LambdaMethodCallDetail();
        lambdaMethodCallDetail.setCallId(lambdaMethodCall.getCallId());
        lambdaMethodCallDetail.setCallerFullMethod(lambdaMethodCall.getCallerFullMethod());
        lambdaMethodCallDetail.setCallerLineNumber(lambdaMethodCall.getCallerLineNumber());
        lambdaMethodCallDetail.setCalleeFullMethod(lambdaMethodCall.getCalleeFullMethod());
        lambdaMethodCallDetail.setLambdaCalleeFullMethod(lambdaMethodCall.getLambdaCalleeFullMethod());
        lambdaMethodCallDetail.setLambdaNextFullMethod(lambdaMethodCall.getLambdaNextFullMethod());
        lambdaMethodCallDetail.setLambdaNextIsStream(lambdaMethodCall.getLambdaNextIsStream());
        lambdaMethodCallDetail.setLambdaNextIsIntermediate(lambdaMethodCall.getLambdaNextIsIntermediate());
        lambdaMethodCallDetail.setLambdaNextIsTerminal(lambdaMethodCall.getLambdaNextIsTerminal());
        return lambdaMethodCallDetail;
    }

    public MethodDetail getCallerFullMethodDetail() {
        return callerFullMethodDetail;
    }

    public void setCallerFullMethodDetail(MethodDetail callerFullMethodDetail) {
        this.callerFullMethodDetail = callerFullMethodDetail;
    }

    public MethodDetail getCalleeFullMethodDetail() {
        return calleeFullMethodDetail;
    }

    public void setCalleeFullMethodDetail(MethodDetail calleeFullMethodDetail) {
        this.calleeFullMethodDetail = calleeFullMethodDetail;
    }

    public MethodDetail getLambdaCalleeFullMethodDetail() {
        return lambdaCalleeFullMethodDetail;
    }

    public void setLambdaCalleeFullMethodDetail(MethodDetail lambdaCalleeFullMethodDetail) {
        this.lambdaCalleeFullMethodDetail = lambdaCalleeFullMethodDetail;
    }

    public MethodDetail getLambdaNextFullMethodDetail() {
        return lambdaNextFullMethodDetail;
    }

    public void setLambdaNextFullMethodDetail(MethodDetail lambdaNextFullMethodDetail) {
        this.lambdaNextFullMethodDetail = lambdaNextFullMethodDetail;
    }
}
