package com.adrninistrator.jacg.extractor.dto.common.extract;

import com.adrninistrator.jacg.dto.callline.CallGraphLineParsed;

/**
 * @author adrninistrator
 * @date 2023/2/20
 * @description: 调用堆栈文件处理后对应行的信息
 */
public abstract class BaseCallGraphExtractedLine {
    // 在调用堆栈文件中的数据序号
    protected int dataSeq;

    // 在调用堆栈文件中的行号
    protected int lineNumber;

    // 当前行的完整内容
    protected String lineContent;

    // 当前行解析后的内容
    protected CallGraphLineParsed callGraphLineParsed;

    // 是否在其他线程中执行
    protected boolean runInOtherThread;

    // 是否在事务中执行
    private boolean runInTransaction;

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

    public CallGraphLineParsed getCallGraphLineParsed() {
        return callGraphLineParsed;
    }

    public void setCallGraphLineParsed(CallGraphLineParsed callGraphLineParsed) {
        this.callGraphLineParsed = callGraphLineParsed;
    }

    public boolean isRunInOtherThread() {
        return runInOtherThread;
    }

    public void setRunInOtherThread(boolean runInOtherThread) {
        this.runInOtherThread = runInOtherThread;
    }

    public boolean isRunInTransaction() {
        return runInTransaction;
    }

    public void setRunInTransaction(boolean runInTransaction) {
        this.runInTransaction = runInTransaction;
    }
}
