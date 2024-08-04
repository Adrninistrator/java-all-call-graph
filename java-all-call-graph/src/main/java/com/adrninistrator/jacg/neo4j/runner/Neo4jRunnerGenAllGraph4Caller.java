package com.adrninistrator.jacg.neo4j.runner;

import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Caller;

/**
 * @author adrninistrator
 * @date 2024/7/23
 * @description: 生成指定方法向下完整调用链，从neo4j读取数据
 */
public class Neo4jRunnerGenAllGraph4Caller extends RunnerGenAllGraph4Caller {

    public Neo4jRunnerGenAllGraph4Caller(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
    }

    @Override
    protected boolean handleDb() {
        return false;
    }

    @Override
    protected boolean useNeo4j() {
        return true;
    }
}
