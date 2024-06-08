package com.adrninistrator.jacg.extractor.dto.springtx.extractfile;

import com.adrninistrator.jacg.extractor.dto.common.extract.BaseCalleeExtractedMethod;
import com.adrninistrator.jacg.extractor.dto.common.extractfile.AbstractCallGraphExtractedFile;
import com.adrninistrator.jacg.extractor.dto.springtx.entrymethod.SpTxEntryMethodTxAnnotation;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/3/2
 * @description: Spring事务调用查询结果的文件信息，使用事务注解
 */
public class SpTxCallByAnnotationFile extends AbstractCallGraphExtractedFile {
    // Spring事务入口方法，使用注解
    private final SpTxEntryMethodTxAnnotation spTxEntryMethodTxAnnotation;

    // 被调用的方法信息列表
    private final List<BaseCalleeExtractedMethod> calleeExtractedMethodList;

    public SpTxCallByAnnotationFile(SpTxEntryMethodTxAnnotation spTxEntryMethodTxAnnotation, List<BaseCalleeExtractedMethod> calleeExtractedMethodList) {
        this.spTxEntryMethodTxAnnotation = spTxEntryMethodTxAnnotation;
        this.calleeExtractedMethodList = calleeExtractedMethodList;
    }

    public SpTxEntryMethodTxAnnotation getSpTxEntryMethodTxAnnotation() {
        return spTxEntryMethodTxAnnotation;
    }

    public List<BaseCalleeExtractedMethod> getCalleeExtractedMethodList() {
        return calleeExtractedMethodList;
    }
}
