package com.adrninistrator.jacg.extractor.dto.result;

import com.adrninistrator.jacg.dto.call_graph_result.CallGraphResultLineParsed;

/**
 * @author adrninistrator
 * @date 2023/2/20
 * @description: 方法完整调用链文件处理结果的方法信息
 */
public abstract class BaseCallGraphResultMethodInfo {
    // 在方法完整调用链文件中的数据序号
    protected int dataSeq;

    // 在方法完整调用链文件中的行号
    protected int lineNumber;

    // 当前行的完整内容
    protected String lineContent;

    // 当前行解析后的内容
    protected CallGraphResultLineParsed callGraphLineParsed;

    public int getDataSeq() {
        return dataSeq;
    }

    public void setDataSeq(int dataSeq) {
        this.dataSeq = dataSeq;
    }

    public int getLineNumber() {
        return lineNumber;
    }

    public void setLineNumber(int lineNumber) {
        this.lineNumber = lineNumber;
    }

    public String getLineContent() {
        return lineContent;
    }

    public void setLineContent(String lineContent) {
        this.lineContent = lineContent;
    }

    public CallGraphResultLineParsed getCallGraphLineParsed() {
        return callGraphLineParsed;
    }

    public void setCallGraphLineParsed(CallGraphResultLineParsed callGraphLineParsed) {
        this.callGraphLineParsed = callGraphLineParsed;
    }
}
