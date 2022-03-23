package com.adrninistrator.jacg.runner;

/**
 * @author adrninistrator
 * @date 2021/6/24
 * @description: 从数据库读取数据，生成指定类调用的所有向下的调用关系，支持忽略特定的包名、类、方法
 */

public class RunnerGenAllGraph4CallerSupportIgnore extends RunnerGenAllGraph4Caller {

    static {
        runner = new RunnerGenAllGraph4CallerSupportIgnore();
    }

    @Override
    public boolean isSupportIgnore() {
        return true;
    }
}
