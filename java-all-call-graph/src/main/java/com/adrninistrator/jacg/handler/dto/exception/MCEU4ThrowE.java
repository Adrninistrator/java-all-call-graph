package com.adrninistrator.jacg.handler.dto.exception;

/**
 * @author adrninistrator
 * @date 2024/1/6
 * @description: 通过throw抛出catch的异常对象
 */
public class MCEU4ThrowE extends BaseMethodCatchExceptionUsage {

    // 通过throw抛出异常的代码行号
    protected int throwLineNumber;

    @Override
    public String getUsageDescription() {
        return "通过throw抛出catch的异常对象";
    }

    @Override
    public String getUsageDetail() {
        return "代码第 " + throwLineNumber + " 行抛出catch的异常对象";
    }

    public int getThrowLineNumber() {
        return throwLineNumber;
    }

    public void setThrowLineNumber(int throwLineNumber) {
        this.throwLineNumber = throwLineNumber;
    }
}
