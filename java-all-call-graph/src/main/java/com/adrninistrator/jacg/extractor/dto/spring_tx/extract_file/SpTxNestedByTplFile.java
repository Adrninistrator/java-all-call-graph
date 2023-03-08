package com.adrninistrator.jacg.extractor.dto.spring_tx.extract_file;

import com.adrninistrator.jacg.extractor.dto.common.extract_file.AbstractCallGraphExtractedFile;
import com.adrninistrator.jacg.extractor.dto.spring_tx.entry_method.SpTxEntryMethodTxTpl;
import com.adrninistrator.jacg.extractor.dto.spring_tx.extract.SpTxCalleeInfo;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/2/24
 * @description: Spring事务嵌套查询结果的文件信息，使用事务模板
 */
public class SpTxNestedByTplFile extends AbstractCallGraphExtractedFile {
    // Spring事务入口方法，使用事务模板
    private final SpTxEntryMethodTxTpl spTxEntryMethodTxTpl;

    // 被调用的事务信息列表
    private final List<SpTxCalleeInfo> spTxCalleeInfoList;

    public SpTxNestedByTplFile(SpTxEntryMethodTxTpl spTxEntryMethodTxTpl, List<SpTxCalleeInfo> spTxCalleeInfoList) {
        this.spTxEntryMethodTxTpl = spTxEntryMethodTxTpl;
        this.spTxCalleeInfoList = spTxCalleeInfoList;
    }

    public SpTxEntryMethodTxTpl getSpTxEntryMethodTxTpl() {
        return spTxEntryMethodTxTpl;
    }

    public List<SpTxCalleeInfo> getSpTxCalleeInfoList() {
        return spTxCalleeInfoList;
    }
}
