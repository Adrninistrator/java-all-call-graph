package com.adrninistrator.jacg.extensions.dto;

/**
 * @author adrninistrator
 * @date 2021/10/19
 * @description:
 */
public class ExtendedDataInfo extends BaseExtendedData {

    // 数据序号
    private int dataSeq;

    // 行号
    private int lineNumber;

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

    @Override
    public String toString() {
        return "ExtendedDataInfo{" +
                "dataType='" + dataType + '\'' +
                ", dataValue='" + dataValue + '\'' +
                ", dataSeq=" + dataSeq +
                ", lineNumber=" + lineNumber +
                '}';
    }
}
