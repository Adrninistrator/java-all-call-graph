package test.composite;

import com.adrninistrator.jacg.findstack.FindCallStackTrace;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Callee;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Caller;
import com.adrninistrator.jacg.runner.RunnerWriteCallGraphFile;
import com.adrninistrator.jacg.runner.RunnerWriteDb;

/**
 * @author adrninistrator
 * @date 2023/4/2
 * @description:
 */
public class TestRunAllUseConfigFile {
    public static void main(String[] args) {
        new RunnerWriteCallGraphFile().run();
        new RunnerWriteDb().run();
        new FindCallStackTrace(true).find();
        new FindCallStackTrace(false).find();
        new RunnerGenAllGraph4Callee().run();
        new RunnerGenAllGraph4Caller().run();
    }
}
