package com.adrninistrator.jacg.handler.dto.string.element;

/**
 * @author adrninistrator
 * @date 2025/11/15
 * @description: 字符串元素，方法调用返回值
 */
public class StringElementMethodCallReturn extends BaseStringElement {

    // 被调用方法
    private String calleeFullMethod;

    public String getCalleeFullMethod() {
        return calleeFullMethod;
    }

    public void setCalleeFullMethod(String calleeFullMethod) {
        this.calleeFullMethod = calleeFullMethod;
    }
}
