package com.adrninistrator.jacg.runner;

/**
 * @author adrninistrator
 * @date 2021/6/24
 * @description:
 */

public class RunnerGenAllGraph4CallerSupportIgnore extends RunnerGenAllGraph4Caller {

    static {
        runner = new RunnerGenAllGraph4CallerSupportIgnore();
    }

    // 是否支持忽略指定方法
    @Override
    protected boolean supportIgnore() {
        return true;
    }
}
