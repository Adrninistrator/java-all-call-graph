package com.adrninistrator.jacg.extractor.dto.spring_tx.result;

import com.adrninistrator.jacg.extractor.common.enums.SpringTxTypeEnum;

/**
 * @author adrninistrator
 * @date 2023/2/24
 * @description: Spring事务查询结果，被调用的事务信息
 */
public class SpTxCalleeResult {
    // 在方法完整调用链文件中的数据序号
    private final int dataSeq;

    // 在方法完整调用链文件中的行号
    private final int lineNumber;

    // 被调用事务使用方式：事务注解/事务模板
    private final SpringTxTypeEnum springTxTypeEnum;

    // 被调用完整方法
    private final String calleeFullMethod;

    // 被调用上一层方法（事务模板）
    private final String calleeUpperFullMethod;

    // 被调用Spring事务注解中的事务传播行为
    private final String txPropagation;

    public SpTxCalleeResult(int dataSeq, int lineNumber, SpringTxTypeEnum springTxTypeEnum, String calleeFullMethod, String calleeUpperFullMethod, String txPropagation) {
        this.dataSeq = dataSeq;
        this.lineNumber = lineNumber;
        this.springTxTypeEnum = springTxTypeEnum;
        this.calleeFullMethod = calleeFullMethod;
        this.calleeUpperFullMethod = calleeUpperFullMethod;
        this.txPropagation = txPropagation;
    }

    public int getDataSeq() {
        return dataSeq;
    }

    public int getLineNumber() {
        return lineNumber;
    }

    public SpringTxTypeEnum getSpringTxTypeEnum() {
        return springTxTypeEnum;
    }

    public String getCalleeFullMethod() {
        return calleeFullMethod;
    }

    public String getCalleeUpperFullMethod() {
        return calleeUpperFullMethod;
    }

    public String getTxPropagation() {
        return txPropagation;
    }
}
