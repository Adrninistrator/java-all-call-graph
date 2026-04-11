package test.jacgconf;

import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Caller;
import com.adrninistrator.javacg2.exceptions.JavaCG2RuntimeException;

/**
 * @author adrninistrator
 * @date 2026/2/16
 * @description: 生成向下的方法完整调用链
 */

public class TestRunnerGenAllGraph4CallerConf {

    public static void main(String[] args) {
        ConfigureWrapper configureWrapper = new ConfigureWrapper(false, ".");
        boolean success = new RunnerGenAllGraph4Caller(configureWrapper).run();
        if (!success) {
            throw new JavaCG2RuntimeException("执行失败");
        }
    }
}
