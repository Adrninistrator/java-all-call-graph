package com.adrninistrator.jacg.extractor.dto.result;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2022/8/29
 * @description:
 */
public class CalleeEntryMethodFile extends BaseResultFile {

    // 调用链数据结果列表
    private List<CalleeEntryMethodInfo> calleeEntryMethodInfoList;

    public List<CalleeEntryMethodInfo> getCalleeEntryMethodInfoList() {
        return calleeEntryMethodInfoList;
    }

    public void setCalleeEntryMethodInfoList(List<CalleeEntryMethodInfo> calleeEntryMethodInfoList) {
        this.calleeEntryMethodInfoList = calleeEntryMethodInfoList;
    }
}
