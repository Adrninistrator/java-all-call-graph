package com.adrninistrator.jacg.reporter.dto.spring_tx;

import com.adrninistrator.jacg.extractor.dto.spring_tx.entry_method.SpTxEntryMethodTxTpl;
import com.adrninistrator.jacg.extractor.dto.spring_tx.extract.SpTxCalleeInfo;

/**
 * @author adrninistrator
 * @date 2023/2/28
 * @description: Spring事务嵌套信息，使用事务模板
 */
public class SpringTxNestedTplReport extends AbstractSpringTxNestedReport {

    // Spring事务入口方法，使用注解
    private SpTxEntryMethodTxTpl spTxEntryMethodTxTpl;

    public SpringTxNestedTplReport(SpTxEntryMethodTxTpl spTxEntryMethodTxTpl, SpTxCalleeInfo spTxCalleeInfo, String filePath) {
        this.spTxEntryMethodTxTpl = spTxEntryMethodTxTpl;
        this.spTxCalleeInfo = spTxCalleeInfo;
        this.stackFilePath = filePath;
    }

    public SpTxEntryMethodTxTpl getSpTxEntryMethodTxTpl() {
        return spTxEntryMethodTxTpl;
    }

    public void setSpTxEntryMethodTxTpl(SpTxEntryMethodTxTpl spTxEntryMethodTxTpl) {
        this.spTxEntryMethodTxTpl = spTxEntryMethodTxTpl;
    }
}
