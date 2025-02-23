package test.jacg;

import com.adrninistrator.jacg.runner.RunnerWriteCallGraphFile;
import com.adrninistrator.javacg2.exceptions.JavaCG2RuntimeException;

/**
 * @author adrninistrator
 * @date 2023/4/2
 * @description:
 */
public class TestRunnerWriteCallGraphFile {

    public static void main(String[] args) {
        boolean success = new RunnerWriteCallGraphFile().run();
        if (!success) {
            throw new JavaCG2RuntimeException("执行失败");
        }
    }
}
