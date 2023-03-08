package com.adrninistrator.jacg.reporter.dto.spring_tx;

import com.adrninistrator.jacg.extractor.dto.spring_tx.entry_method.SpTxEntryMethodTxAnnotation;
import com.adrninistrator.jacg.extractor.dto.spring_tx.extract.SpTxCalleeInfo;

/**
 * @author adrninistrator
 * @date 2023/2/28
 * @description: Spring事务嵌套信息，使用事务注解
 */
public class SpringTxNestedAnnotationReport extends AbstractSpringTxNestedReport {

    // Spring事务入口方法，使用注解
    private SpTxEntryMethodTxAnnotation spTxEntryMethodTxAnnotation;

    public SpringTxNestedAnnotationReport(SpTxEntryMethodTxAnnotation spTxEntryMethodTxAnnotation, SpTxCalleeInfo spTxCalleeInfo, String filePath) {
        this.spTxEntryMethodTxAnnotation = spTxEntryMethodTxAnnotation;
        this.spTxCalleeInfo = spTxCalleeInfo;
        this.stackFilePath = filePath;
    }

    public SpTxEntryMethodTxAnnotation getSpTxEntryMethodTxAnnotation() {
        return spTxEntryMethodTxAnnotation;
    }

    public void setSpTxEntryMethodTxAnnotation(SpTxEntryMethodTxAnnotation spTxEntryMethodTxAnnotation) {
        this.spTxEntryMethodTxAnnotation = spTxEntryMethodTxAnnotation;
    }
}
