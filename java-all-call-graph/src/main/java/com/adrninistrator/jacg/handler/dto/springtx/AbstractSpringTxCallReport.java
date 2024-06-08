package com.adrninistrator.jacg.handler.dto.springtx;

import com.adrninistrator.jacg.extractor.dto.common.extract.BaseCalleeExtractedMethod;
import com.adrninistrator.jacg.handler.dto.reporter.AbstractReportInfo;

/**
 * @author adrninistrator
 * @date 2023/3/2
 * @description: Spring事务调用信息，抽象类
 */
public abstract class AbstractSpringTxCallReport extends AbstractReportInfo {

    // 被调用的事务信息
    protected BaseCalleeExtractedMethod calleeExtractedMethod;

    public BaseCalleeExtractedMethod getCalleeExtractedMethod() {
        return calleeExtractedMethod;
    }

    public void setCalleeExtractedMethod(BaseCalleeExtractedMethod calleeExtractedMethod) {
        this.calleeExtractedMethod = calleeExtractedMethod;
    }
}
