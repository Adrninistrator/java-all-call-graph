package com.adrninistrator.jacg.handler.dto.spring;

/**
 * @author adrninistrator
 * @date 2023/3/6
 * @description: Spring非法的事务注解方法调用
 */
public class SpringInvalidTxAnnotationMethodCall {
    // 调用方完整方法
    private final String callerFullMethod;

    // 调用方代码行号
    private final int callerLineNumber;

    // 被调用方完整方法
    private final String calleeFullMethod;

    // 调用方法是否使用了Spring事务注解
    private final boolean callerWithSpringTx;

    // 调用方法事务注解的Spring事务传播行为
    private final String callerTxPropagation;

    // 被调用方法事务注解的Spring事务传播行为
    private final String calleeTxPropagation;

    public SpringInvalidTxAnnotationMethodCall(String callerFullMethod, int callerLineNumber, String calleeFullMethod, boolean callerWithSpringTx, String callerTxPropagation,
                                               String calleeTxPropagation) {
        this.callerFullMethod = callerFullMethod;
        this.callerLineNumber = callerLineNumber;
        this.calleeFullMethod = calleeFullMethod;
        this.callerWithSpringTx = callerWithSpringTx;
        this.callerTxPropagation = callerTxPropagation;
        this.calleeTxPropagation = calleeTxPropagation;
    }

    public String getCallerFullMethod() {
        return callerFullMethod;
    }

    public int getCallerLineNumber() {
        return callerLineNumber;
    }

    public String getCalleeFullMethod() {
        return calleeFullMethod;
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
