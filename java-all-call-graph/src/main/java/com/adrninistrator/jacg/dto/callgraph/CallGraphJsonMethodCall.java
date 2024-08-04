package com.adrninistrator.jacg.dto.callgraph;

/**
 * @author adrninistrator
 * @date 2024/7/19
 * @description: 生成的调用链的JSON格式表示，方法调用
 */
public class CallGraphJsonMethodCall {

    // 方法调用层级
    private int level;

    // 调用方方法HASH+字节数
    private String callerMethodHash;

    // 被调用方方法HASH+字节数
    private String calleeMethodHash;

    // 调用方代码行号
    private int callerLineNumber;

    public CallGraphJsonMethodCall() {
    }

    public CallGraphJsonMethodCall(int level, String callerMethodHash, String calleeMethodHash, int callerLineNumber) {
        this.level = level;
        this.callerMethodHash = callerMethodHash;
        this.calleeMethodHash = calleeMethodHash;
        this.callerLineNumber = callerLineNumber;
    }

    public int getLevel() {
        return level;
    }

    public void setLevel(int level) {
        this.level = level;
    }

    public String getCallerMethodHash() {
        return callerMethodHash;
    }

    public void setCallerMethodHash(String callerMethodHash) {
        this.callerMethodHash = callerMethodHash;
    }

    public String getCalleeMethodHash() {
        return calleeMethodHash;
    }

    public void setCalleeMethodHash(String calleeMethodHash) {
        this.calleeMethodHash = calleeMethodHash;
    }

    public int getCallerLineNumber() {
        return callerLineNumber;
    }

    public void setCallerLineNumber(int callerLineNumber) {
        this.callerLineNumber = callerLineNumber;
    }
}
