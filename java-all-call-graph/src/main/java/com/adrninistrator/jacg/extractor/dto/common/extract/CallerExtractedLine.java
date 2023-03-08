package com.adrninistrator.jacg.extractor.dto.common.extract;

import com.adrninistrator.jacg.dto.call_line.CallGraphLineParsed;

/**
 * @author adrninistrator
 * @date 2021/10/19
 * @description: 向下的调用堆栈文件处理后对应行的信息
 */
public class CallerExtractedLine extends BaseCallGraphExtractedLine {

    // 上一行的完整内容
    private String lastLineContent;

    // 上一行解析后的内容
    protected CallGraphLineParsed lastLineParsed;

    public String getLastLineContent() {
        return lastLineContent;
    }

    public void setLastLineContent(String lastLineContent) {
        this.lastLineContent = lastLineContent;
    }

    public CallGraphLineParsed getLastLineParsed() {
        return lastLineParsed;
    }

    public void setLastLineParsed(CallGraphLineParsed lastLineParsed) {
        this.lastLineParsed = lastLineParsed;
    }

    @Override
    public String toString() {
        return "CallerExtractedLine{" +
                "dataSeq=" + dataSeq +
                ", lineNumber=" + lineNumber +
                ", lineContent='" + lineContent + '\'' +
                '}';
    }
}
