package com.adrninistrator.jacg.dto.callgraph;

import com.adrninistrator.jacg.util.JACGClassMethodUtil;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2024/7/19
 * @description: 生成的调用链的JSON格式表示
 */
public class CallGraphJson {

    // 方法调用List
    private final List<CallGraphJsonMethodCall> methodCalls = new ArrayList<>();

    /*
        方法信息Map
        key       方法HASH+字节数
        value     方法信息
     */
    private final Map<String, CallGraphJsonMethod> methodMap = new HashMap<>();

    public void addMethodCall(CallGraphJsonMethodCall callGraphJsonMethodCall) {
        methodCalls.add(callGraphJsonMethodCall);
    }

    public void addMethod(CallGraphJsonMethod method) {
        String methodHash = JACGClassMethodUtil.genMethodHashWithLen(method.getMethodDetail().getFullMethod(), method.getMethodDetail().getReturnType());
        methodMap.put(methodHash, method);
    }

    public List<CallGraphJsonMethodCall> getMethodCalls() {
        return methodCalls;
    }

    public Map<String, CallGraphJsonMethod> getMethodMap() {
        return methodMap;
    }
}
