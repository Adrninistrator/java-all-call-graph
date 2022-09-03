package com.adrninistrator.jacg.extractor.dto.result;

/**
 * @author adrninistrator
 * @date 2022/8/29
 * @description:
 */
public class CalleeEntryMethodInfo {

    // 数据序号
    private int dataSeq;

    // 行号
    private int lineNumber;

    // 当前行的完整内容
    private String lineContent;

    // 当前行的完整方法（不包含注解）
    private String fullMethod;

    // 下一行的完整内容
    private String nextLineContent;

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

    public String getFullMethod() {
        return fullMethod;
    }

    public void setFullMethod(String fullMethod) {
        this.fullMethod = fullMethod;
    }

    public String getNextLineContent() {
        return nextLineContent;
    }

    public void setNextLineContent(String nextLineContent) {
        this.nextLineContent = nextLineContent;
    }
}
