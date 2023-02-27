package com.adrninistrator.jacg.extractor.dto.spring_tx.result;

import com.adrninistrator.jacg.dto.call_graph_result.AbstractCallGraphResultFileInfo;
import com.adrninistrator.jacg.extractor.dto.spring_tx.entry_method.SpTxEntryMethodTxTpl;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/2/24
 * @description: Spring事务嵌套查询结果的文件信息，使用事务模板
 */
public class SpTxNestedResultTxTplFileInfo extends AbstractCallGraphResultFileInfo {
    // Spring事务入口方法，使用事务模板
    private final SpTxEntryMethodTxTpl spTxEntryMethodTxTpl;

    // 被调用的事务信息列表
    private final List<SpTxCalleeResult> spTxCalleeResultList;

    public SpTxNestedResultTxTplFileInfo(SpTxEntryMethodTxTpl spTxEntryMethodTxTpl, List<SpTxCalleeResult> spTxCalleeResultList) {
        this.spTxEntryMethodTxTpl = spTxEntryMethodTxTpl;
        this.spTxCalleeResultList = spTxCalleeResultList;
    }

    public SpTxEntryMethodTxTpl getSpTxEntryMethodTxTpl() {
        return spTxEntryMethodTxTpl;
    }

    public List<SpTxCalleeResult> getSpTxCalleeResultList() {
        return spTxCalleeResultList;
    }
}
