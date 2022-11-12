package com.adrninistrator.jacg.extractor.dto.result;

import com.adrninistrator.jacg.extensions.dto.extened_data.BaseExtendedData;

/**
 * @author adrninistrator
 * @date 2021/10/19
 * @description:
 */
public class CallerExtendedDataInfo extends BaseExtendedData {
    // 数据序号
    private int dataSeq;

    // 行号
    private int lineNumber;

    // 上一行的完整内容
    private String lastLineContent;

    // 上一行的方法
    private String lastLineFullMethod;

    // 当前行的完整内容
    private String lineContent;

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

    public String getLastLineContent() {
        return lastLineContent;
    }

    public void setLastLineContent(String lastLineContent) {
        this.lastLineContent = lastLineContent;
    }

    public String getLastLineFullMethod() {
        return lastLineFullMethod;
    }

    public void setLastLineFullMethod(String lastLineFullMethod) {
        this.lastLineFullMethod = lastLineFullMethod;
    }

    public String getLineContent() {
        return lineContent;
    }

    public void setLineContent(String lineContent) {
        this.lineContent = lineContent;
    }

    @Override
    public String toString() {
        return "ExtendedDataInfo{" +
                "dataType='" + dataType + '\'' +
                ", dataValue='" + dataValue + '\'' +
                ", dataSeq=" + dataSeq +
                ", lineNumber=" + lineNumber +
                ", lastLineContent='" + lastLineContent + '\'' +
                ", lastLineFullMethod='" + lastLineFullMethod + '\'' +
                ", lineContent='" + lineContent + '\'' +
                '}';
    }
}
