package com.adrninistrator.jacg.extractor.dto.spring_tx.result;

import com.adrninistrator.jacg.dto.call_graph_result.AbstractCallGraphResultFileInfo;
import com.adrninistrator.jacg.extractor.dto.spring_tx.entry_method.SpTxEntryMethodTxAnnotation;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/2/24
 * @description: Spring事务嵌套查询结果的文件信息，使用事务注解
 */
public class SpTxNestedResultTxAnnotationFileInfo extends AbstractCallGraphResultFileInfo {
    // Spring事务入口方法，使用注解
    private final SpTxEntryMethodTxAnnotation spTxEntryMethodTxAnnotation;

    // 被调用的事务信息列表
    private final List<SpTxCalleeResult> spTxCalleeResultList;

    public SpTxNestedResultTxAnnotationFileInfo(SpTxEntryMethodTxAnnotation spTxEntryMethodTxAnnotation, List<SpTxCalleeResult> spTxCalleeResultList) {
        this.spTxEntryMethodTxAnnotation = spTxEntryMethodTxAnnotation;
        this.spTxCalleeResultList = spTxCalleeResultList;
    }

    public SpTxEntryMethodTxAnnotation getSpTxEntryMethodTxAnnotation() {
        return spTxEntryMethodTxAnnotation;
    }

    public List<SpTxCalleeResult> getSpTxCalleeResultList() {
        return spTxCalleeResultList;
    }
}
