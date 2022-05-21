package com.adrninistrator.jacg.dto.notice;

/**
 * @author adrninistrator
 * @date 2021/8/1
 * @description:
 */

public class NoticeCallInfo {

    private String callerMethodHash;

    private String callerFullMethod;

    private String calleeFullMethod;

    public String getCallerMethodHash() {
        return callerMethodHash;
    }

    public void setCallerMethodHash(String callerMethodHash) {
        this.callerMethodHash = callerMethodHash;
    }

    public String getCallerFullMethod() {
        return callerFullMethod;
    }

    public void setCallerFullMethod(String callerFullMethod) {
        this.callerFullMethod = callerFullMethod;
    }

    public String getCalleeFullMethod() {
        return calleeFullMethod;
    }

    public void setCalleeFullMethod(String calleeFullMethod) {
        this.calleeFullMethod = calleeFullMethod;
    }
}
