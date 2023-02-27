package com.adrninistrator.jacg.extractor.dto.spring_tx.result;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/2/24
 * @description: Spring事务嵌套查询结果，合并后的信息
 */
public class SpTxNestedResultFileInfoCombined {
    // 使用事务注解的文件信息列表
    private final List<SpTxNestedResultTxAnnotationFileInfo> spTxNestedResultTxAnnotationFileInfoList;

    // 使用事务模板的文件信息列表
    private final List<SpTxNestedResultTxTplFileInfo> spTxNestedResultTxTplFileInfoList;

    public SpTxNestedResultFileInfoCombined(List<SpTxNestedResultTxAnnotationFileInfo> spTxNestedResultTxAnnotationFileInfoList,
                                            List<SpTxNestedResultTxTplFileInfo> spTxNestedResultTxTplFileInfoList) {
        this.spTxNestedResultTxAnnotationFileInfoList = spTxNestedResultTxAnnotationFileInfoList;
        this.spTxNestedResultTxTplFileInfoList = spTxNestedResultTxTplFileInfoList;
    }

    public List<SpTxNestedResultTxAnnotationFileInfo> getSpTxNestedResultTxAnnotationFileInfoList() {
        return spTxNestedResultTxAnnotationFileInfoList;
    }

    public List<SpTxNestedResultTxTplFileInfo> getSpTxNestedResultTxTplFileInfoList() {
        return spTxNestedResultTxTplFileInfoList;
    }
}
