package com.adrninistrator.jacg.dto.write_db;

/**
 * @author adrninistrator
 * @date 2023/1/10
 * @description: 用于写入数据库的数据，Lambda表达式方法信息
 */
public class WriteDbData4LambdaMethodInfo extends AbstractWriteDbData {
    private int callId;
    private String lambdaCalleeClassName;
    private String lambdaCalleeMethodName;
    private String lambdaCalleeFullMethod;
    /*
        以下字段可能为null
     */
    private String lambdaNextClassName;
    private String lambdaNextMethodName;
    private String lambdaNextFullMethod;
    private Boolean lambdaNextIsStream;
    private Boolean lambdaNextIsIntermediate;
    private Boolean lambdaNextIsTerminal;

    public int getCallId() {
        return callId;
    }

    public void setCallId(int callId) {
        this.callId = callId;
    }

    public String getLambdaCalleeClassName() {
        return lambdaCalleeClassName;
    }

    public void setLambdaCalleeClassName(String lambdaCalleeClassName) {
        this.lambdaCalleeClassName = lambdaCalleeClassName;
    }

    public String getLambdaCalleeMethodName() {
        return lambdaCalleeMethodName;
    }

    public void setLambdaCalleeMethodName(String lambdaCalleeMethodName) {
        this.lambdaCalleeMethodName = lambdaCalleeMethodName;
    }

    public String getLambdaCalleeFullMethod() {
        return lambdaCalleeFullMethod;
    }

    public void setLambdaCalleeFullMethod(String lambdaCalleeFullMethod) {
        this.lambdaCalleeFullMethod = lambdaCalleeFullMethod;
    }

    public String getLambdaNextClassName() {
        return lambdaNextClassName;
    }

    public void setLambdaNextClassName(String lambdaNextClassName) {
        this.lambdaNextClassName = lambdaNextClassName;
    }

    public String getLambdaNextMethodName() {
        return lambdaNextMethodName;
    }

    public void setLambdaNextMethodName(String lambdaNextMethodName) {
        this.lambdaNextMethodName = lambdaNextMethodName;
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
}
