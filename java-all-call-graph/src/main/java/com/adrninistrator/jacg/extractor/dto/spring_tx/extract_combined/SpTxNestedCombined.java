package com.adrninistrator.jacg.extractor.dto.spring_tx.extract_combined;

import com.adrninistrator.jacg.extractor.dto.spring_tx.extract_file.SpTxNestedByAnnotationFile;
import com.adrninistrator.jacg.extractor.dto.spring_tx.extract_file.SpTxNestedByTplFile;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/2/24
 * @description: Spring事务嵌套查询结果，合并后的信息
 */
public class SpTxNestedCombined {
    // 使用事务注解的文件信息列表
    private final List<SpTxNestedByAnnotationFile> spTxNestedByAnnotationFileList;

    // 使用事务模板的文件信息列表
    private final List<SpTxNestedByTplFile> spTxNestedByTplFileList;

    public SpTxNestedCombined(List<SpTxNestedByAnnotationFile> spTxNestedByAnnotationFileList,
                              List<SpTxNestedByTplFile> spTxNestedByTplFileList) {
        this.spTxNestedByAnnotationFileList = spTxNestedByAnnotationFileList;
        this.spTxNestedByTplFileList = spTxNestedByTplFileList;
    }

    public List<SpTxNestedByAnnotationFile> getSpTxNestedByAnnotationFileList() {
        return spTxNestedByAnnotationFileList;
    }

    public List<SpTxNestedByTplFile> getSpTxNestedByTplFileList() {
        return spTxNestedByTplFileList;
    }
}
