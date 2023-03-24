package com.adrninistrator.jacg.extractor.dto.common.extract;

/**
 * @author adrninistrator
 * @date 2023/3/2
 * @description: 被调用的方法信息基类
 */
public class BaseCalleeExtractedMethod {
    // 在调用堆栈文件中的数据序号
    protected int dataSeq;

    // 在调用堆栈文件中的行号
    protected int lineNumber;

    // 被调用完整方法
    protected String calleeFullMethod;

    // 被调用上一层方法
    protected String calleeUpperFullMethod;

    // 在其他线程执行
    protected boolean runInOtherThread;

    // 在事务中执行
    protected boolean runInTransaction;

    public BaseCalleeExtractedMethod(int dataSeq, int lineNumber, String calleeFullMethod, String calleeUpperFullMethod, boolean runInOtherThread, boolean runInTransaction) {
        this.dataSeq = dataSeq;
        this.lineNumber = lineNumber;
        this.calleeFullMethod = calleeFullMethod;
        this.calleeUpperFullMethod = calleeUpperFullMethod;
        this.runInOtherThread = runInOtherThread;
        this.runInTransaction = runInTransaction;
    }

    public int getDataSeq() {
        return dataSeq;
    }

    public int getLineNumber() {
        return lineNumber;
    }

    public String getCalleeFullMethod() {
        return calleeFullMethod;
    }

    public String getCalleeUpperFullMethod() {
        return calleeUpperFullMethod;
    }

    public boolean isRunInOtherThread() {
        return runInOtherThread;
    }

    public boolean isRunInTransaction() {
        return runInTransaction;
    }
}
