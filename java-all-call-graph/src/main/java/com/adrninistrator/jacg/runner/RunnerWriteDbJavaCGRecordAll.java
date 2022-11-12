package com.adrninistrator.jacg.runner;

/**
 * @author adrninistrator
 * @date 2021/11/2
 * @description: 将通过java-callgraph2生成的直接调用关系文件写入数据库，记录所有的接口调用实现类，及子类调用父类方法
 */
public class RunnerWriteDbJavaCGRecordAll extends RunnerWriteDb {

    @Override
    public boolean isJavaCGRecordAll() {
        return true;
    }
}
