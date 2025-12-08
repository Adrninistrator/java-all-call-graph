package com.adrninistrator.jacg.dto.multiple;

import com.adrninistrator.jacg.dto.method.FullMethodWithReturnType;

import java.util.Set;

/**
 * @author adrninistrator
 * @date 2021/8/1
 * @description:
 */

public class MultiCallInfo {
    private final String callerMethodHash;

    private final String callerFullMethodWithReturnType;

    private final Set<FullMethodWithReturnType> calleeMethodSet;

    public MultiCallInfo(String callerMethodHash, String callerFullMethodWithReturnType, Set<FullMethodWithReturnType> calleeMethodSet) {
        this.callerMethodHash = callerMethodHash;
        this.callerFullMethodWithReturnType = callerFullMethodWithReturnType;
        this.calleeMethodSet = calleeMethodSet;
    }

    public String getCallerMethodHash() {
        return callerMethodHash;
    }

    public String getCallerFullMethodWithReturnType() {
        return callerFullMethodWithReturnType;
    }

    public Set<FullMethodWithReturnType> getCalleeMethodSet() {
        return calleeMethodSet;
    }
}
