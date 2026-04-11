package test.jacgconf;

import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.jardiff.runner.RunnerGenJarDiffCalleeGraph;
import com.adrninistrator.javacg2.conf.JavaCG2ConfigureWrapper;
import com.adrninistrator.javacg2.exceptions.JavaCG2RuntimeException;

/**
 * @author adrninistrator
 * @date 2026/2/16
 * @description:
 */
public class TestRunnerGenJarDiffCalleeGraphConf {

    public static void main(String[] args) {
        JavaCG2ConfigureWrapper javaCG2ConfigureWrapper = new JavaCG2ConfigureWrapper(false, ".");
        ConfigureWrapper configureWrapper = new ConfigureWrapper(false, ".");
        RunnerGenJarDiffCalleeGraph runnerGenJarDiffCalleeGraph = new RunnerGenJarDiffCalleeGraph(javaCG2ConfigureWrapper, configureWrapper, null, null);
        if (!runnerGenJarDiffCalleeGraph.run()) {
            throw new JavaCG2RuntimeException("执行失败");
        }
    }
}
