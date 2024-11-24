package com.adrninistrator.jacg.extractor.dto.common.extract;

import com.adrninistrator.jacg.dto.callline.CallGraphLineParsed;

/**
 * @author adrninistrator
 * @date 2021/10/19
 * @description: 向下的调用堆栈文件处理后对应行的信息
 */
public class CallerExtractedLine extends BaseCallGraphExtractedLine {
    // 找到的被调用方法的直接调用方法对应行的完整内容
    private String directlyCallerLineContent;

    // 找到的被调用方法的直接调用方法对应行解析后的内容
    protected CallGraphLineParsed directlyCalleeLineParsed;

    public String getDirectlyCallerLineContent() {
        return directlyCallerLineContent;
    }

    public void setDirectlyCallerLineContent(String directlyCallerLineContent) {
        this.directlyCallerLineContent = directlyCallerLineContent;
    }

    public CallGraphLineParsed getDirectlyCalleeLineParsed() {
        return directlyCalleeLineParsed;
    }

    public void setDirectlyCalleeLineParsed(CallGraphLineParsed directlyCalleeLineParsed) {
        this.directlyCalleeLineParsed = directlyCalleeLineParsed;
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
