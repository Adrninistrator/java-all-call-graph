package com.adrninistrator.jacg.handler.dto.exception;

/**
 * @author adrninistrator
 * @date 2024/1/9
 * @description: catch的异常对象未被使用时，catch代码块中通过throw抛出其他异常
 */
public class NoMCEU4MethodThrow extends BaseMethodCatchExceptionUsage {

    // throw的代码行号
    private int throwLineNumber;

    // throw的异常类型
    private String throwExceptionType;

    @Override
    public String getUsageDescription() {
        return "catch的异常对象未被使用时，catch代码块中通过throw抛出其他异常";
    }

    @Override
    public String getUsageDetail() {
        return "代码第 " + throwLineNumber + " 行抛出了类型为 " + throwExceptionType + " 的异常";
    }

    public int getThrowLineNumber() {
        return throwLineNumber;
    }

    public void setThrowLineNumber(int throwLineNumber) {
        this.throwLineNumber = throwLineNumber;
    }

    public String getThrowExceptionType() {
        return throwExceptionType;
    }

    public void setThrowExceptionType(String throwExceptionType) {
        this.throwExceptionType = throwExceptionType;
    }
}
