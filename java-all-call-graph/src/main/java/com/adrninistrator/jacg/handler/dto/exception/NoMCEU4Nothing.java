package com.adrninistrator.jacg.handler.dto.exception;

/**
 * @author adrninistrator
 * @date 2024/1/10
 * @description: catch的异常对象未被使用
 */
public class NoMCEU4Nothing extends BaseMethodCatchExceptionUsage {
    @Override
    public String getUsageDescription() {
        return "catch的异常对象未被使用";
    }

    @Override
    public String getUsageDetail() {
        return "";
    }
}
