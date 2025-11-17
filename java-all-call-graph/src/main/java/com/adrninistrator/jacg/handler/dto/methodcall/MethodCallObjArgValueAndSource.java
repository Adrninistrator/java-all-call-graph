package com.adrninistrator.jacg.handler.dto.methodcall;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2025/9/7
 * @description: 方法调用中被调用对象与参数对应的常量值或来源（包括使用方法调用的返回值，或方法参数）
 */
public class MethodCallObjArgValueAndSource {

    // 包含的数据类型Set
    private final Set<String> methodCallInfoTypeSet = new HashSet<>();

    // 对应的常量值列表
    private List<String> constantValueList;

    // 对应的使用方法调用的返回值的方法调用ID列表
    private List<Integer> useMethodCallReturnCallIdList;

    // 对应的使用方法参数序号列表
    private List<Integer> useMethodArgSeqList;

    public Set<String> getMethodCallInfoTypeSet() {
        return methodCallInfoTypeSet;
    }

    public List<String> getConstantValueList() {
        return constantValueList;
    }

    public void setConstantValueList(List<String> constantValueList) {
        this.constantValueList = constantValueList;
    }

    public List<Integer> getUseMethodCallReturnCallIdList() {
        return useMethodCallReturnCallIdList;
    }

    public void setUseMethodCallReturnCallIdList(List<Integer> useMethodCallReturnCallIdList) {
        this.useMethodCallReturnCallIdList = useMethodCallReturnCallIdList;
    }

    public List<Integer> getUseMethodArgSeqList() {
        return useMethodArgSeqList;
    }

    public void setUseMethodArgSeqList(List<Integer> useMethodArgSeqList) {
        this.useMethodArgSeqList = useMethodArgSeqList;
    }
}
