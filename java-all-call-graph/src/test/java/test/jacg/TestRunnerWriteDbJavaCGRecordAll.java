package test.jacg;

import com.adrninistrator.jacg.runner.RunnerWriteDbJavaCGRecordAll;

/**
 * @author adrninistrator
 * @date 2021/11/2
 * @description: 读取jar包内容，生成方法调用关系，并写入数据库，记录所有的接口调用实现类，及子类调用父类方法
 */
public class TestRunnerWriteDbJavaCGRecordAll {

    public static void main(String[] args) {
        new RunnerWriteDbJavaCGRecordAll().run();
    }
}
