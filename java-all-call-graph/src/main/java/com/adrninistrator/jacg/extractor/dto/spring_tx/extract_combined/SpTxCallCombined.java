package com.adrninistrator.jacg.extractor.dto.spring_tx.extract_combined;

import com.adrninistrator.jacg.extractor.dto.spring_tx.extract_file.SpTxCallByAnnotationFile;
import com.adrninistrator.jacg.extractor.dto.spring_tx.extract_file.SpTxCallByTplFile;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/3/2
 * @description: Spring事务调用查询结果，合并后的信息
 */
public class SpTxCallCombined {
    // 使用事务注解的文件信息列表
    private final List<SpTxCallByAnnotationFile> spTxCallByAnnotationFileList;

    // 使用事务模板的文件信息列表
    private final List<SpTxCallByTplFile> spTxCallByTplFileList;

    public SpTxCallCombined(List<SpTxCallByAnnotationFile> spTxCallByAnnotationFileList,
                            List<SpTxCallByTplFile> spTxCallByTplFileList) {
        this.spTxCallByAnnotationFileList = spTxCallByAnnotationFileList;
        this.spTxCallByTplFileList = spTxCallByTplFileList;
    }

    public List<SpTxCallByAnnotationFile> getSpTxCallByAnnotationFileList() {
        return spTxCallByAnnotationFileList;
    }

    public List<SpTxCallByTplFile> getSpTxCallByTplFileList() {
        return spTxCallByTplFileList;
    }
}
