package com.adrninistrator.jacg.handler.dto.springtx;

import com.adrninistrator.jacg.extractor.dto.common.extract.BaseCalleeExtractedMethod;
import com.adrninistrator.jacg.extractor.dto.springtx.entrymethod.SpTxEntryMethodTxAnnotation;

/**
 * @author adrninistrator
 * @date 2023/3/2
 * @description: Spring事务调用信息，使用事务注解
 */
public class SpringTxCallAnnotationReport extends AbstractSpringTxCallReport {

    // Spring事务入口方法，使用注解
    private SpTxEntryMethodTxAnnotation spTxEntryMethodTxAnnotation;

    public SpringTxCallAnnotationReport(SpTxEntryMethodTxAnnotation spTxEntryMethodTxAnnotation, BaseCalleeExtractedMethod calleeExtractedMethod, String filePath) {
        this.spTxEntryMethodTxAnnotation = spTxEntryMethodTxAnnotation;
        this.calleeExtractedMethod = calleeExtractedMethod;
        this.stackFilePath = filePath;
    }

    public SpTxEntryMethodTxAnnotation getSpTxEntryMethodTxAnnotation() {
        return spTxEntryMethodTxAnnotation;
    }

    public void setSpTxEntryMethodTxAnnotation(SpTxEntryMethodTxAnnotation spTxEntryMethodTxAnnotation) {
        this.spTxEntryMethodTxAnnotation = spTxEntryMethodTxAnnotation;
    }
}
