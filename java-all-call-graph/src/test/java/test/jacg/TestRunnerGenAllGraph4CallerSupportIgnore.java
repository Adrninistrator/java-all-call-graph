package test.jacg;

import com.adrninistrator.jacg.runner.RunnerGenAllGraph4CallerSupportIgnore;

/**
 * @author adrninistrator
 * @date 2021/6/24
 * @description: 生成向下的方法完整调用链，支持忽略特定的包名、类、方法
 */

public class TestRunnerGenAllGraph4CallerSupportIgnore {

    public static void main(String[] args) {
        new RunnerGenAllGraph4CallerSupportIgnore().run();
    }
}
