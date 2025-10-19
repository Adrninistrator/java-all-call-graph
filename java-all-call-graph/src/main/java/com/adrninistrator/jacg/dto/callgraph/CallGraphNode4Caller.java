package com.adrninistrator.jacg.dto.callgraph;

import java.util.Map;

/**
 * @author adrninistrator
 * @date 2021/6/18
 * @description: 生成指定类调用的所有向下的调用关系时，使用的栈中的节点
 */

public class CallGraphNode4Caller {
    // 当前调用方法HASH+长度
    private final String callerMethodHash;

    // 当前被调用方法的call_id
    private int methodCallId;

    // 当前调用方法的完整方法
    private final String callerFullMethod;

    // 当前调用方法的被调用方法数量
    private int calleeMethodNum = 0;

    /*
        key 需要将被调用对象类型替换为方法参数类型的方法参数序号
        value   需要将被调用对象类型替换为方法参数类型的方法参数类型
     */
    private Map<Integer, String> replaceCalleeTypeArgSeqTypeMap;

    public CallGraphNode4Caller(String callerMethodHash, int methodCallId, String callerFullMethod) {
        this.callerMethodHash = callerMethodHash;
        this.methodCallId = methodCallId;
        this.callerFullMethod = callerFullMethod;
    }

    @Override
    public String toString() {
        return calleeMethodNum + " " + callerFullMethod;
    }

    // 当前调用方法的被调用方法数量加1
    public void addCallerMethodNum() {
        calleeMethodNum++;
    }

    public String getCallerMethodHash() {
        return callerMethodHash;
    }

    public int getMethodCallId() {
        return methodCallId;
    }

    public void setMethodCallId(int methodCallId) {
        this.methodCallId = methodCallId;
    }

    public String getCallerFullMethod() {
        return callerFullMethod;
    }

    public int getCalleeMethodNum() {
        return calleeMethodNum;
    }

    public void setCalleeMethodNum(int calleeMethodNum) {
        this.calleeMethodNum = calleeMethodNum;
    }

    public Map<Integer, String> getReplaceCalleeTypeArgSeqTypeMap() {
        return replaceCalleeTypeArgSeqTypeMap;
    }

    public void setReplaceCalleeTypeArgSeqTypeMap(Map<Integer, String> replaceCalleeTypeArgSeqTypeMap) {
        this.replaceCalleeTypeArgSeqTypeMap = replaceCalleeTypeArgSeqTypeMap;
    }
}
