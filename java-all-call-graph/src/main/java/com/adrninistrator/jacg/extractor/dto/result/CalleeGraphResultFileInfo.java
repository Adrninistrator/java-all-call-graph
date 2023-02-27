package com.adrninistrator.jacg.extractor.dto.result;

import com.adrninistrator.jacg.dto.call_graph_result.AbstractCallGraphResultFileInfo;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2022/8/29
 * @description: 向上的方法完整调用链文件处理结果的文件信息
 */
public class CalleeGraphResultFileInfo extends AbstractCallGraphResultFileInfo {
    // 方法完整调用链文件处理结果的信息列表
    private final List<CalleeGraphResultMethodInfo> calleeGraphResultMethodInfoList;

    public CalleeGraphResultFileInfo(List<CalleeGraphResultMethodInfo> calleeGraphResultMethodInfoList) {
        this.calleeGraphResultMethodInfoList = calleeGraphResultMethodInfoList;
    }

    public List<CalleeGraphResultMethodInfo> getCalleeGraphResultMethodInfoList() {
        return calleeGraphResultMethodInfoList;
    }

    @Override
    public String toString() {
        return fullMethod + " [" + calleeGraphResultMethodInfoList.size() + "]";
    }
}
