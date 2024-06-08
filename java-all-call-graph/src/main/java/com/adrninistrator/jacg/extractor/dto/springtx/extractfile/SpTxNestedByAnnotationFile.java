package com.adrninistrator.jacg.extractor.dto.springtx.extractfile;

import com.adrninistrator.jacg.extractor.dto.common.extractfile.AbstractCallGraphExtractedFile;
import com.adrninistrator.jacg.extractor.dto.springtx.entrymethod.SpTxEntryMethodTxAnnotation;
import com.adrninistrator.jacg.extractor.dto.springtx.extract.SpTxCalleeInfo;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/2/24
 * @description: Spring事务嵌套查询结果的文件信息，使用事务注解
 */
public class SpTxNestedByAnnotationFile extends AbstractCallGraphExtractedFile {
    // Spring事务入口方法，使用注解
    private final SpTxEntryMethodTxAnnotation spTxEntryMethodTxAnnotation;

    // 被调用的事务信息列表
    private final List<SpTxCalleeInfo> spTxCalleeInfoList;

    public SpTxNestedByAnnotationFile(SpTxEntryMethodTxAnnotation spTxEntryMethodTxAnnotation, List<SpTxCalleeInfo> spTxCalleeInfoList) {
        this.spTxEntryMethodTxAnnotation = spTxEntryMethodTxAnnotation;
        this.spTxCalleeInfoList = spTxCalleeInfoList;
    }

    public SpTxEntryMethodTxAnnotation getSpTxEntryMethodTxAnnotation() {
        return spTxEntryMethodTxAnnotation;
    }

    public List<SpTxCalleeInfo> getSpTxCalleeInfoList() {
        return spTxCalleeInfoList;
    }
}
