package com.adrninistrator.jacg.extractor.dto.springtx.extract;

import com.adrninistrator.jacg.extractor.common.enums.SpringTxTypeEnum;
import com.adrninistrator.jacg.extractor.dto.common.extract.BaseCalleeExtractedMethod;

/**
 * @author adrninistrator
 * @date 2023/2/24
 * @description: Spring事务查询结果，被调用的事务信息
 */
public class SpTxCalleeInfo extends BaseCalleeExtractedMethod {
    // 被调用事务使用方式：事务注解/事务模板
    private SpringTxTypeEnum springTxTypeEnum;

    // 被调用Spring事务注解中的事务传播行为
    private String txPropagation;

    public SpTxCalleeInfo(
            int dataSeq,
            int lineNumber,
            String calleeFullMethod,
            String calleeUpperFullMethod,
            boolean runInOtherThread,
            SpringTxTypeEnum springTxTypeEnum,
            String txPropagation) {
        super(dataSeq, lineNumber, calleeFullMethod, calleeUpperFullMethod, runInOtherThread, false);

        this.springTxTypeEnum = springTxTypeEnum;
        this.txPropagation = txPropagation;
    }

    public SpringTxTypeEnum getSpringTxTypeEnum() {
        return springTxTypeEnum;
    }

    public void setSpringTxTypeEnum(SpringTxTypeEnum springTxTypeEnum) {
        this.springTxTypeEnum = springTxTypeEnum;
    }

    public String getTxPropagation() {
        return txPropagation;
    }

    public void setTxPropagation(String txPropagation) {
        this.txPropagation = txPropagation;
    }
}
