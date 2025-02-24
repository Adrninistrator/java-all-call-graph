package com.adrninistrator.jacg.handler.dto.methodcall;

import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCall;

import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2025/2/22
 * @description: 方法调用，及对应的被调用对象、参数的值，支持枚举
 */
public class MethodCallWithValueSupportEnum {

    // 方法调用
    private WriteDbData4MethodCall methodCall;

    /*
        被调用对象、参数的值的Map
        key     被调用对象、参数的序号
        value   被调用对象、参数的值列表
     */
    private Map<Integer, List<MethodCallObjArgValue>> methodCallObjArgValueMap;

    public WriteDbData4MethodCall getMethodCall() {
        return methodCall;
    }

    public void setMethodCall(WriteDbData4MethodCall methodCall) {
        this.methodCall = methodCall;
    }

    public Map<Integer, List<MethodCallObjArgValue>> getMethodCallObjArgValueMap() {
        return methodCallObjArgValueMap;
    }

    public void setMethodCallObjArgValueMap(Map<Integer, List<MethodCallObjArgValue>> methodCallObjArgValueMap) {
        this.methodCallObjArgValueMap = methodCallObjArgValueMap;
    }
}
