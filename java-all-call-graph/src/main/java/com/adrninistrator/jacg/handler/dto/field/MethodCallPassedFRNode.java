package com.adrninistrator.jacg.handler.dto.field;

import com.adrninistrator.jacg.dto.methodcall.parsed.AbstractMethodCallInfoParsed;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCall;

import java.util.ArrayList;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/12/3
 * @description: 处理通过方法调用传递的字段关联关系时使用的栈的节点
 */
public class MethodCallPassedFRNode {

    // 当前处理的set方法对应的方法调用列表
    private final List<WriteDbData4MethodCall> methodCallList;

    // 当前处理的set方法对应的方法调用解析后信息列表
    private final List<AbstractMethodCallInfoParsed> methodCallInfoParsedList;

    /*
        当前处理的set方法对应的方法调用解析后信息列表索引
        节点列表序号初始化为-1，因为在处理时需要加一
     */
    private int methodCallInfoParsedListIndex = -1;

    public MethodCallPassedFRNode(List<WriteDbData4MethodCall> methodCallList, List<AbstractMethodCallInfoParsed> methodCallInfoParsedList) {
        this.methodCallList = methodCallList;
        this.methodCallInfoParsedList = methodCallInfoParsedList;
    }

    public MethodCallPassedFRNode(WriteDbData4MethodCall methodCall, List<AbstractMethodCallInfoParsed> methodCallInfoParsedList) {
        methodCallList = new ArrayList<>(methodCallInfoParsedList.size());
        for (int i = 0; i < methodCallInfoParsedList.size(); i++) {
            methodCallList.add(methodCall);
        }
        this.methodCallInfoParsedList = methodCallInfoParsedList;
    }

    /**
     * 获取当前处理的set方法对应的方法调用解析后信息
     *
     * @return
     */
    public AbstractMethodCallInfoParsed getCurrentMethodCallInfoParsed() {
        if (methodCallInfoParsedListIndex < 0) {
            return methodCallInfoParsedList.get(0);
        }

        return methodCallInfoParsedList.get(methodCallInfoParsedListIndex);
    }

    /**
     * 获取当前处理的set方法对应的方法调用
     *
     * @return
     */
    public WriteDbData4MethodCall getCurrentMethodCall() {
        if (methodCallInfoParsedListIndex < 0) {
            return methodCallList.get(0);
        }

        return methodCallList.get(methodCallInfoParsedListIndex);
    }

    /**
     * 增加当前处理的set方法对应的方法调用解析后信息列表序号
     *
     * @return true: 增加成功 false: 已处理到列表最后一个元素，增加失败
     */
    public boolean addMethodCallInfoParsedListIndex() {
        if (methodCallInfoParsedListIndex + 1 < methodCallInfoParsedList.size()) {
            methodCallInfoParsedListIndex++;
            return true;
        }
        return false;
    }
}
