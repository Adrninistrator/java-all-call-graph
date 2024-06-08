package com.adrninistrator.jacg.handler.dto.springtx;

import com.adrninistrator.jacg.extractor.dto.springtx.extract.SpTxCalleeInfo;
import com.adrninistrator.jacg.handler.dto.reporter.AbstractReportInfo;

/**
 * @author adrninistrator
 * @date 2023/3/1
 * @description: Spring事务嵌套信息，抽象类
 */
public abstract class AbstractSpringTxNestedReport extends AbstractReportInfo {

    // 被调用的事务信息
    protected SpTxCalleeInfo spTxCalleeInfo;

    public SpTxCalleeInfo getSpTxCalleeInfo() {
        return spTxCalleeInfo;
    }

    public void setSpTxCalleeInfo(SpTxCalleeInfo spTxCalleeInfo) {
        this.spTxCalleeInfo = spTxCalleeInfo;
    }
}
