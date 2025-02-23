package test.jacg;

import com.adrninistrator.jacg.diff.runner.RunnerGenJarDiffCalleeGraph;
import com.adrninistrator.javacg2.exceptions.JavaCG2RuntimeException;

/**
 * @author adrninistrator
 * @date 2024/3/9
 * @description:
 */
public class TestRunnerGenJarDiffCalleeGraph {

    public static void main(String[] args) {
        boolean success =  new RunnerGenJarDiffCalleeGraph().generate();
        if (!success) {
            throw new JavaCG2RuntimeException("执行失败");
        }
    }
}
