package com.adrninistrator.jacg.extractor.dto.result;

import com.adrninistrator.jacg.dto.call_graph_result.AbstractCallGraphResultFileInfo;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2021/10/19
 * @description: 向下的方法完整调用链文件处理结果的文件信息
 */
public class CallerGraphResultFileInfo extends AbstractCallGraphResultFileInfo {
    // 方法完整调用链文件处理结果的信息列表
    private final List<CallerGraphResultMethodInfo> callerGraphResultMethodInfoList;

    public CallerGraphResultFileInfo(List<CallerGraphResultMethodInfo> callerGraphResultMethodInfoList) {
        this.callerGraphResultMethodInfoList = callerGraphResultMethodInfoList;
    }

    public List<CallerGraphResultMethodInfo> getCallerGraphResultMethodInfoList() {
        return callerGraphResultMethodInfoList;
    }

    @Override
    public String toString() {
        return fullMethod + " [" + callerGraphResultMethodInfoList.size() + "]";
    }
}
