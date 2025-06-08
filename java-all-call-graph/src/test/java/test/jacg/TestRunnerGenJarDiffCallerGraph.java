package test.jacg;

import com.adrninistrator.jacg.jardiff.dto.result.JarDiffResult;
import com.adrninistrator.jacg.jardiff.runner.RunnerGenJarDiffCallerGraph;
import com.adrninistrator.javacg2.exceptions.JavaCG2RuntimeException;

/**
 * @author adrninistrator
 * @date 2025/5/17
 * @description:
 */
public class TestRunnerGenJarDiffCallerGraph {

    public static void main(String[] args) {
        JarDiffResult jarDiffResult = new RunnerGenJarDiffCallerGraph().generate();
        if (!jarDiffResult.isSuccess()) {
            throw new JavaCG2RuntimeException("执行失败");
        }
    }
}
