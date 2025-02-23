package test.jacg;

import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Callee;
import com.adrninistrator.javacg2.exceptions.JavaCG2RuntimeException;

/**
 * @author adrninistrator
 * @date 2021/6/23
 * @description: 生成向上的方法完整调用链
 */

public class TestRunnerGenAllGraph4Callee {

    public static void main(String[] args) {
        boolean success = new RunnerGenAllGraph4Callee().run();
        if (!success) {
            throw new JavaCG2RuntimeException("执行失败");
        }
    }
}
