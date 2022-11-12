package test.manual_add_call_graph;

import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Caller;
import org.junit.Test;

/**
 * @author adrninistrator
 * @date 2022/4/22
 * @description:
 */
public class TestMACGRunnerGenAllGraph4Caller extends TestMACGBase {
    @Test
    public void test() {
        new RunnerGenAllGraph4Caller().run(configureWrapper);
    }
}
