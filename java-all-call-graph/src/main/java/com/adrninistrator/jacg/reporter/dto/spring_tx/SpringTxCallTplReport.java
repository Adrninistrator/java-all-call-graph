package com.adrninistrator.jacg.reporter.dto.spring_tx;

import com.adrninistrator.jacg.extractor.dto.common.extract.BaseCalleeExtractedMethod;
import com.adrninistrator.jacg.extractor.dto.spring_tx.entry_method.SpTxEntryMethodTxTpl;

/**
 * @author adrninistrator
 * @date 2023/3/2
 * @description: Spring事务调用信息，使用事务模板
 */
public class SpringTxCallTplReport extends AbstractSpringTxCallReport {

    // Spring事务入口方法，使用注解
    private SpTxEntryMethodTxTpl spTxEntryMethodTxTpl;

    public SpringTxCallTplReport(SpTxEntryMethodTxTpl spTxEntryMethodTxTpl, BaseCalleeExtractedMethod calleeExtractedMethod, String filePath) {
        this.spTxEntryMethodTxTpl = spTxEntryMethodTxTpl;
        this.calleeExtractedMethod = calleeExtractedMethod;
        this.stackFilePath = filePath;
    }

    public SpTxEntryMethodTxTpl getSpTxEntryMethodTxTpl() {
        return spTxEntryMethodTxTpl;
    }

    public void setSpTxEntryMethodTxTpl(SpTxEntryMethodTxTpl spTxEntryMethodTxTpl) {
        this.spTxEntryMethodTxTpl = spTxEntryMethodTxTpl;
    }
}
