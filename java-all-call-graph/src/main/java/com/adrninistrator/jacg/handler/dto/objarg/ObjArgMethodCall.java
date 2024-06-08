package com.adrninistrator.jacg.handler.dto.objarg;

/**
 * @author adrninistrator
 * @date 2023/10/31
 * @description: 方法调用中被调用对象或参数的信息，对应方法调用
 */
public class ObjArgMethodCall extends AbstractObjArgInfo {
    // 方法调用序号
    private final int methodCallId;

    public ObjArgMethodCall(int methodCallId) {
        this.methodCallId = methodCallId;
    }

    public int getMethodCallId() {
        return methodCallId;
    }
}
