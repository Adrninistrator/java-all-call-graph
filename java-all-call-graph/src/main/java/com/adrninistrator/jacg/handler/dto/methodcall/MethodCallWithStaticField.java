package com.adrninistrator.jacg.handler.dto.methodcall;

import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCall;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCallStaticField;

/**
 * @author adrninistrator
 * @date 2024/2/3
 * @description: 方法调用使用静态字段及对应的方法调用信息
 */
public class MethodCallWithStaticField {

    private final WriteDbData4MethodCall methodCall;

    private final WriteDbData4MethodCallStaticField methodCallStaticField;

    public MethodCallWithStaticField(WriteDbData4MethodCall methodCall, WriteDbData4MethodCallStaticField methodCallStaticField) {
        this.methodCall = methodCall;
        this.methodCallStaticField = methodCallStaticField;
    }

    public WriteDbData4MethodCall getMethodCall() {
        return methodCall;
    }

    public WriteDbData4MethodCallStaticField getMethodCallStaticField() {
        return methodCallStaticField;
    }
}
