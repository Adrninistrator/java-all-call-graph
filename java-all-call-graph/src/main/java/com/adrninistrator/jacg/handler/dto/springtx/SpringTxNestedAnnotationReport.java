package com.adrninistrator.jacg.handler.dto.springtx;

import com.adrninistrator.jacg.extractor.dto.springtx.entrymethod.SpTxEntryMethodTxAnnotation;
import com.adrninistrator.jacg.extractor.dto.springtx.extract.SpTxCalleeInfo;

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
