package com.adrninistrator.jacg.reporter.dto.base;

/**
 * @author adrninistrator
 * @date 2023/3/2
 * @description: 报告相关信息的基类
 */
public abstract class AbstractReportInfo {
    // 调用堆栈文件路径
    protected String stackFilePath;

    public String getStackFilePath() {
        return stackFilePath;
    }

    public void setStackFilePath(String stackFilePath) {
        this.stackFilePath = stackFilePath;
    }
}
