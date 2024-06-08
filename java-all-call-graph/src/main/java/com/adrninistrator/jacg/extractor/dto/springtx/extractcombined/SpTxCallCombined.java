package com.adrninistrator.jacg.extractor.dto.springtx.extractcombined;

import com.adrninistrator.jacg.common.list.ListWithResult;
import com.adrninistrator.jacg.extractor.dto.springtx.extractfile.SpTxCallByAnnotationFile;
import com.adrninistrator.jacg.extractor.dto.springtx.extractfile.SpTxCallByTplFile;

/**
 * @author adrninistrator
 * @date 2023/3/2
 * @description: Spring事务调用查询结果，合并后的信息
 */
public class SpTxCallCombined {
    // 使用事务注解的文件信息列表
    private final ListWithResult<SpTxCallByAnnotationFile> spTxCallByAnnotationFileList;

    // 使用事务模板的文件信息列表
    private final ListWithResult<SpTxCallByTplFile> spTxCallByTplFileList;

    public SpTxCallCombined(ListWithResult<SpTxCallByAnnotationFile> spTxCallByAnnotationFileList,
                            ListWithResult<SpTxCallByTplFile> spTxCallByTplFileList) {
        this.spTxCallByAnnotationFileList = spTxCallByAnnotationFileList;
        this.spTxCallByTplFileList = spTxCallByTplFileList;
    }

    public ListWithResult<SpTxCallByAnnotationFile> getSpTxCallByAnnotationFileList() {
        return spTxCallByAnnotationFileList;
    }

    public ListWithResult<SpTxCallByTplFile> getSpTxCallByTplFileList() {
        return spTxCallByTplFileList;
    }
}
