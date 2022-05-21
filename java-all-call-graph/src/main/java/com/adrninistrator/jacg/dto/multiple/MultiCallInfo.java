package com.adrninistrator.jacg.dto.multiple;

import java.util.Set;

/**
 * @author adrninistrator
 * @date 2021/8/1
 * @description:
 */

public class MultiCallInfo {

    private String callerMethodHash;

    private Set<String> calleeFullMethodSet;

    public String getCallerMethodHash() {
        return callerMethodHash;
    }

    public void setCallerMethodHash(String callerMethodHash) {
        this.callerMethodHash = callerMethodHash;
    }

    public Set<String> getCalleeFullMethodSet() {
        return calleeFullMethodSet;
    }

    public void setCalleeFullMethodSet(Set<String> calleeFullMethodSet) {
        this.calleeFullMethodSet = calleeFullMethodSet;
    }
}
