package test.composite;

import com.adrninistrator.jacg.find_stack.FindCallStackTrace;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Callee;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Caller;
import com.adrninistrator.jacg.runner.RunnerWriteCallGraphFile;
import com.adrninistrator.jacg.runner.RunnerWriteDb;

/**
 * @author adrninistrator
 * @date 2023/4/2
 * @description:
 */
public class TestRunAll {
    public static void main(String[] args) {
        new FindCallStackTrace().find(true);
        new FindCallStackTrace().find(false);
        new RunnerGenAllGraph4Callee().run();
        new RunnerGenAllGraph4Caller().run();
        new RunnerWriteCallGraphFile().run();
        new RunnerWriteDb().run();
    }
}
