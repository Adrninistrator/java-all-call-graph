package com.adrninistrator.jacg.extractor.dto.springtx.extractcombined;

import com.adrninistrator.jacg.common.list.ListWithResult;
import com.adrninistrator.jacg.extractor.dto.springtx.extractfile.SpTxNestedByAnnotationFile;
import com.adrninistrator.jacg.extractor.dto.springtx.extractfile.SpTxNestedByTplFile;

/**
 * @author adrninistrator
 * @date 2023/2/24
 * @description: Spring事务嵌套查询结果，合并后的信息
 */
public class SpTxNestedCombined {
    // 使用事务注解的文件信息列表
    private final ListWithResult<SpTxNestedByAnnotationFile> spTxNestedByAnnotationFileList;

    // 使用事务模板的文件信息列表
    private final ListWithResult<SpTxNestedByTplFile> spTxNestedByTplFileList;

    public SpTxNestedCombined(ListWithResult<SpTxNestedByAnnotationFile> spTxNestedByAnnotationFileList,
                              ListWithResult<SpTxNestedByTplFile> spTxNestedByTplFileList) {
        this.spTxNestedByAnnotationFileList = spTxNestedByAnnotationFileList;
        this.spTxNestedByTplFileList = spTxNestedByTplFileList;
    }

    public ListWithResult<SpTxNestedByAnnotationFile> getSpTxNestedByAnnotationFileList() {
        return spTxNestedByAnnotationFileList;
    }

    public ListWithResult<SpTxNestedByTplFile> getSpTxNestedByTplFileList() {
        return spTxNestedByTplFileList;
    }
}
