package com.adrninistrator.jacg.dto.callgraph;

import java.util.Map;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2025/10/15
 * @description: 生成向下方法完整调用链时，指定哪些方法参数作为被调用对象涉及多态时的类型替换使用的信息
 */
public class CalleeArgTypePolymorphismInfo {

    // 需要处理的方法参数序号集合
    private Set<Integer> argSeqSet;

    /*
        key 被调用对象为方法参数的方法调用序号
        value   方法调用使用的方法参数序号
     */
    private Map<Integer, Integer> callIdArgSeqMap;

    // 被调用方法HASH
    private String calleeMethodHash;

    // 被调用完整方法
    private String calleeFullMethod;

    // 被调用方法返回类型
    private String calleeReturnType;

    public Set<Integer> getArgSeqSet() {
        return argSeqSet;
    }

    public void setArgSeqSet(Set<Integer> argSeqSet) {
        this.argSeqSet = argSeqSet;
    }

    public Map<Integer, Integer> getCallIdArgSeqMap() {
        return callIdArgSeqMap;
    }

    public void setCallIdArgSeqMap(Map<Integer, Integer> callIdArgSeqMap) {
        this.callIdArgSeqMap = callIdArgSeqMap;
    }

    public String getCalleeMethodHash() {
        return calleeMethodHash;
    }

    public void setCalleeMethodHash(String calleeMethodHash) {
        this.calleeMethodHash = calleeMethodHash;
    }

    public String getCalleeFullMethod() {
        return calleeFullMethod;
    }

    public void setCalleeFullMethod(String calleeFullMethod) {
        this.calleeFullMethod = calleeFullMethod;
    }

    public String getCalleeReturnType() {
        return calleeReturnType;
    }

    public void setCalleeReturnType(String calleeReturnType) {
        this.calleeReturnType = calleeReturnType;
    }
}
