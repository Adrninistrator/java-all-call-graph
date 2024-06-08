package com.adrninistrator.jacg.handler.dto.spring;

import com.adrninistrator.jacg.dto.methodcall.MethodCallPair;

/**
 * @author adrninistrator
 * @date 2023/3/6
 * @description: Spring非法的事务注解方法调用
 */
public class SpringInvalidTxAnnotationMethodCall extends MethodCallPair {

    // 调用方法是否使用了Spring事务注解
    private final boolean callerWithSpringTx;

    // 调用方法事务注解的Spring事务传播行为
    private final String callerTxPropagation;

    // 被调用方法事务注解的Spring事务传播行为
    private final String calleeTxPropagation;

    public SpringInvalidTxAnnotationMethodCall(String callerFullMethod, int callerLineNumber, String calleeFullMethod, boolean callerWithSpringTx, String callerTxPropagation,
                                               String calleeTxPropagation) {
        super(callerFullMethod, callerLineNumber, calleeFullMethod);
        this.callerWithSpringTx = callerWithSpringTx;
        this.callerTxPropagation = callerTxPropagation;
        this.calleeTxPropagation = calleeTxPropagation;
    }

    public boolean isCallerWithSpringTx() {
        return callerWithSpringTx;
    }

    public String getCallerTxPropagation() {
        return callerTxPropagation;
    }

    public String getCalleeTxPropagation() {
        return calleeTxPropagation;
    }
}
