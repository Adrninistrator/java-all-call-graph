package com.adrninistrator.jacg.handler.dto.spring;

import com.adrninistrator.jacg.dto.method_call.MethodCallPair;

/**
 * @author adrninistrator
 * @date 2023/3/6
 * @description: Spring非法的事务注解方法调用
 */
public class SpringInvalidTxAnnotationMethodCall extends MethodCallPair {

    // 方法事务注解的Spring事务传播行为
    private final String txPropagation;

    public SpringInvalidTxAnnotationMethodCall(String callerFullMethod, int callerLineNumber, String calleeFullMethod, String txPropagation) {
        super(callerFullMethod, callerLineNumber, calleeFullMethod);
        this.txPropagation = txPropagation;
    }

    public String getTxPropagation() {
        return txPropagation;
    }
}
