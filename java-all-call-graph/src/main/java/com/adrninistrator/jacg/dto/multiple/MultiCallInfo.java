package com.adrninistrator.jacg.dto.multiple;

import java.util.Set;

/**
 * @author adrninistrator
 * @date 2021/8/1
 * @description:
 */

public class MultiCallInfo {
    private final String callerMethodHash;

    private final Set<String> calleeFullMethodSet;

    public MultiCallInfo(String callerMethodHash, Set<String> calleeFullMethodSet) {
        this.callerMethodHash = callerMethodHash;
        this.calleeFullMethodSet = calleeFullMethodSet;
    }

    public String getCallerMethodHash() {
        return callerMethodHash;
    }

    public Set<String> getCalleeFullMethodSet() {
        return calleeFullMethodSet;
    }
}
