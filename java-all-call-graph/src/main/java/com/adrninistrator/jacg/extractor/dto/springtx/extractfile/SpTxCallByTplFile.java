package com.adrninistrator.jacg.extractor.dto.springtx.extractfile;

import com.adrninistrator.jacg.extractor.dto.common.extract.BaseCalleeExtractedMethod;
import com.adrninistrator.jacg.extractor.dto.common.extractfile.AbstractCallGraphExtractedFile;
import com.adrninistrator.jacg.extractor.dto.springtx.entrymethod.SpTxEntryMethodTxTpl;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/3/2
 * @description: Spring事务调用查询结果的文件信息，使用事务模板
 */
public class SpTxCallByTplFile extends AbstractCallGraphExtractedFile {
    // Spring事务入口方法，使用事务模板
    private final SpTxEntryMethodTxTpl spTxEntryMethodTxTpl;

    // 被调用的方法信息列表
    private final List<BaseCalleeExtractedMethod> calleeExtractedMethodList;

    public SpTxCallByTplFile(SpTxEntryMethodTxTpl spTxEntryMethodTxTpl, List<BaseCalleeExtractedMethod> calleeExtractedMethodList) {
        this.spTxEntryMethodTxTpl = spTxEntryMethodTxTpl;
        this.calleeExtractedMethodList = calleeExtractedMethodList;
    }

    public SpTxEntryMethodTxTpl getSpTxEntryMethodTxTpl() {
        return spTxEntryMethodTxTpl;
    }

    public List<BaseCalleeExtractedMethod> getCalleeExtractedMethodList() {
        return calleeExtractedMethodList;
    }
}
