package com.adrninistrator.jacg.extractor.dto.result;

import com.adrninistrator.jacg.dto.call_graph_result.CallGraphResultLineParsed;

/**
 * @author adrninistrator
 * @date 2022/8/29
 * @description: 向上的方法完整调用链文件处理结果的方法信息
 */
public class CalleeGraphResultMethodInfo extends BaseCallGraphResultMethodInfo {
    // 下一行的完整内容
    private String nextLineContent;

    // 下一行解析后的内容
    private CallGraphResultLineParsed nextLineParsed;

    public String getNextLineContent() {
        return nextLineContent;
    }

    public void setNextLineContent(String nextLineContent) {
        this.nextLineContent = nextLineContent;
    }

    public CallGraphResultLineParsed getNextLineParsed() {
        return nextLineParsed;
    }

    public void setNextLineParsed(CallGraphResultLineParsed nextLineParsed) {
        this.nextLineParsed = nextLineParsed;
    }

    @Override
    public String toString() {
        return "CalleeGraphResultMethodInfo{" +
                "dataSeq=" + dataSeq +
                ", lineNumber=" + lineNumber +
                ", lineContent='" + lineContent + '\'' +
                '}';
    }
}
