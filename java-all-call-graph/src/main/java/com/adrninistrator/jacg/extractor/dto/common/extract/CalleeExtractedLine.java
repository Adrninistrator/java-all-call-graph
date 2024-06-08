package com.adrninistrator.jacg.extractor.dto.common.extract;

import com.adrninistrator.jacg.dto.callline.CallGraphLineParsed;

/**
 * @author adrninistrator
 * @date 2022/8/29
 * @description: 向上的调用堆栈文件处理后对应行的信息
 */
public class CalleeExtractedLine extends BaseCallGraphExtractedLine {
    // 下一行的完整内容
    private String nextLineContent;

    // 下一行解析后的内容
    private CallGraphLineParsed nextLineParsed;

    public String getNextLineContent() {
        return nextLineContent;
    }

    public void setNextLineContent(String nextLineContent) {
        this.nextLineContent = nextLineContent;
    }

    public CallGraphLineParsed getNextLineParsed() {
        return nextLineParsed;
    }

    public void setNextLineParsed(CallGraphLineParsed nextLineParsed) {
        this.nextLineParsed = nextLineParsed;
    }

    @Override
    public String toString() {
        return "CalleeExtractedLine{" +
                "dataSeq=" + dataSeq +
                ", lineNumber=" + lineNumber +
                ", lineContent='" + lineContent + '\'' +
                '}';
    }
}
