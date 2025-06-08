package com.adrninistrator.jacg.handler.dto.methodcall;

import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCall;
import com.adrninistrator.jacg.dto.writedb.base.BaseWriteDbData;

/**
 * @author adrninistrator
 * @date 2025/5/31
 * @description: 方法调用及方法调用时使用的信息
 */
public class MethodCallWithInfo<T extends BaseWriteDbData> {

    // 方法调用
    private final WriteDbData4MethodCall methodCall;

    // 方法调用时使用的信息
    private final T info;

    public MethodCallWithInfo(WriteDbData4MethodCall methodCall, T info) {
        this.methodCall = methodCall;
        this.info = info;
    }

    public WriteDbData4MethodCall getMethodCall() {
        return methodCall;
    }

    public T getInfo() {
        return info;
    }
}
