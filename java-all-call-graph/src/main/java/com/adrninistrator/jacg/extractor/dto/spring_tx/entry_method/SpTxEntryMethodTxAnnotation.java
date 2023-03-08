package com.adrninistrator.jacg.extractor.dto.spring_tx.entry_method;

/**
 * @author adrninistrator
 * @date 2023/2/23
 * @description: Spring事务入口方法，使用注解
 */
public class SpTxEntryMethodTxAnnotation {
    // 事务注解所在的入口方法
    private final String callFullMethod;

    // 入口方法事务注解的Spring事务传播行为
    private final String txPropagation;

    public SpTxEntryMethodTxAnnotation(String callFullMethod, String txPropagation) {
        this.callFullMethod = callFullMethod;
        this.txPropagation = txPropagation;
    }

    public String getCallFullMethod() {
        return callFullMethod;
    }

    public String getTxPropagation() {
        return txPropagation;
    }
}
