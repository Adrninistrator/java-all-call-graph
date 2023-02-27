package com.adrninistrator.jacg.dto.call_graph;

/**
 * @author adrninistrator
 * @date 2021/6/18
 * @description: 生成指定类调用的所有向下的调用关系时，使用的临时节点
 */

public class CallGraphNode4Caller {
    // 当前调用方法HASH+长度
    private final String callerMethodHash;

    // 当前被调用方法的call_id
    private int methodCallId;

    // 当前调用方法的完整方法
    private final String callerFullMethod;

    // 当前调用方法的被调用方法数量
    private int callerMethodNum = 0;

    public CallGraphNode4Caller(String callerMethodHash, int methodCallId, String callerFullMethod) {
        this.callerMethodHash = callerMethodHash;
        this.methodCallId = methodCallId;
        this.callerFullMethod = callerFullMethod;
    }

    @Override
    public String toString() {
        return callerMethodNum + " " + callerFullMethod;
    }

    // 当前调用方法的被调用方法数量加1
    public void addCallerMethodNum() {
        callerMethodNum++;
    }

    // get
    public String getCallerMethodHash() {
        return callerMethodHash;
    }

    public int getMethodCallId() {
        return methodCallId;
    }

    public String getCallerFullMethod() {
        return callerFullMethod;
    }

    public int getCallerMethodNum() {
        return callerMethodNum;
    }

    // set
    public void setMethodCallId(int methodCallId) {
        this.methodCallId = methodCallId;
    }
}
