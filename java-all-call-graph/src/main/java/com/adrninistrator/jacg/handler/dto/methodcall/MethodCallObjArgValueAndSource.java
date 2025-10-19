package com.adrninistrator.jacg.handler.dto.methodcall;

import com.adrninistrator.javacg2.common.enums.JavaCG2MethodCallInfoTypeEnum;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2025/9/7
 * @description: 方法调用中被调用对象与参数对应的常量值或来源（包括使用方法调用的返回值，或方法参数）
 */
public class MethodCallObjArgValueAndSource {

    // 是否包含以下多种类型的数据
    private boolean containsMultiType;

    // 仅包含以下一种类型的数据时，对应的数据类型
    private JavaCG2MethodCallInfoTypeEnum methodCallInfoTypeEnum;

    // 仅包含以下一种类型的数据时的数量
    private int oneTypeDataNum;

    // 对应的常量值列表
    private List<String> constantValueList;

    // 对应的使用方法调用的返回值的方法调用ID列表
    private List<Integer> useMethodCallReturnCallIdList;

    // 对应的使用方法参数序号列表
    private List<Integer> useMethodArgSeqList;

    public boolean isContainsMultiType() {
        return containsMultiType;
    }

    public void setContainsMultiType(boolean containsMultiType) {
        this.containsMultiType = containsMultiType;
    }

    public JavaCG2MethodCallInfoTypeEnum getMethodCallInfoTypeEnum() {
        return methodCallInfoTypeEnum;
    }

    public void setMethodCallInfoTypeEnum(JavaCG2MethodCallInfoTypeEnum methodCallInfoTypeEnum) {
        this.methodCallInfoTypeEnum = methodCallInfoTypeEnum;
    }

    public int getOneTypeDataNum() {
        return oneTypeDataNum;
    }

    public void setOneTypeDataNum(int oneTypeDataNum) {
        this.oneTypeDataNum = oneTypeDataNum;
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
