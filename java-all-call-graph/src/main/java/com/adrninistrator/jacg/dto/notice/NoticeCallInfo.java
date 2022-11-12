package com.adrninistrator.jacg.dto.notice;

/**
 * @author adrninistrator
 * @date 2021/8/1
 * @description:
 */

public class NoticeCallInfo {
    private final String callerMethodHash;

    private final String callerFullMethod;

    private final String calleeFullMethod;

    public NoticeCallInfo(String callerMethodHash, String callerFullMethod, String calleeFullMethod) {
        this.callerMethodHash = callerMethodHash;
        this.callerFullMethod = callerFullMethod;
        this.calleeFullMethod = calleeFullMethod;
    }

    public String getCallerMethodHash() {
        return callerMethodHash;
    }

    public String getCallerFullMethod() {
        return callerFullMethod;
    }

    public String getCalleeFullMethod() {
        return calleeFullMethod;
    }
}
