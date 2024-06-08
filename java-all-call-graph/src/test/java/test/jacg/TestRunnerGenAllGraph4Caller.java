package test.jacg;

import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Caller;
import org.junit.Assert;

/**
 * @author adrninistrator
 * @date 2021/6/23
 * @description: 生成向下的方法完整调用链
 */

public class TestRunnerGenAllGraph4Caller {

    public static void main(String[] args) {
        Assert.assertTrue(new RunnerGenAllGraph4Caller().run());
    }
}
