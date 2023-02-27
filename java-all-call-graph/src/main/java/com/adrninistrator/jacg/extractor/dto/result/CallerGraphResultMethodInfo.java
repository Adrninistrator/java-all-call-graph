package com.adrninistrator.jacg.extractor.dto.result;

import com.adrninistrator.jacg.dto.call_graph_result.CallGraphResultLineParsed;

/**
 * @author adrninistrator
 * @date 2021/10/19
 * @description: 向下的方法完整调用链文件处理结果的方法信息
 */
public class CallerGraphResultMethodInfo extends BaseCallGraphResultMethodInfo {

    // 上一行的完整内容
    private String lastLineContent;

    // 上一行解析后的内容
    protected CallGraphResultLineParsed lastLineParsed;

    public String getLastLineContent() {
        return lastLineContent;
    }

    public void setLastLineContent(String lastLineContent) {
        this.lastLineContent = lastLineContent;
    }

    public CallGraphResultLineParsed getLastLineParsed() {
        return lastLineParsed;
    }

    public void setLastLineParsed(CallGraphResultLineParsed lastLineParsed) {
        this.lastLineParsed = lastLineParsed;
    }

    @Override
    public String toString() {
        return "CallerGraphResultMethodInfo{" +
                "dataSeq=" + dataSeq +
                ", lineNumber=" + lineNumber +
                ", lineContent='" + lineContent + '\'' +
                '}';
    }
}
