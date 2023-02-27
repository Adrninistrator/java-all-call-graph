package com.adrninistrator.jacg.extensions.method_call_add;

import com.adrninistrator.jacg.dto.method.MethodCallFullMethod;

/**
 * @author adrninistrator
 * @date 2022/11/19
 * @description: 人工添加方法调用关系接口
 */
public interface MethodCallAddInterface {

    /**
     * 处理方法调用
     *
     * @param callerFullMethod
     * @param calleeFullMethod
     * @param calleeClassName
     * @return null代表不需要增加方法调用关系 非null代表需要增加方法调用关系
     */
    MethodCallFullMethod handleMethodCall(String callerFullMethod, String calleeFullMethod, String calleeClassName);
}
