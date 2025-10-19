package test.jacg;

import com.adrninistrator.jacg.jardiff.runner.RunnerGenJarDiffCalleeGraph;
import com.adrninistrator.javacg2.exceptions.JavaCG2RuntimeException;

/**
 * @author adrninistrator
 * @date 2024/3/9
 * @description:
 */
public class TestRunnerGenJarDiffCalleeGraph {

    public static void main(String[] args) {
        RunnerGenJarDiffCalleeGraph runnerGenJarDiffCalleeGraph = new RunnerGenJarDiffCalleeGraph();
        if (!runnerGenJarDiffCalleeGraph.run()) {
            throw new JavaCG2RuntimeException("执行失败");
        }
    }
}
