package com.adrninistrator.jacg.extractor.dto.spring_tx.entry_method;

/**
 * @author adrninistrator
 * @date 2023/2/23
 * @description: Spring事务入口方法，使用注解
 */
public class SpTxEntryMethodTxAnnotation {
    // @Transactional注解所在的完整方法
    private final String callFullMethod;

    // @Transactional注解的Spring事务传播行为
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
