package test.jacg;

import com.adrninistrator.jacg.runner.RunnerWriteCallGraphFile;
import org.junit.Assert;

/**
 * @author adrninistrator
 * @date 2023/4/2
 * @description:
 */
public class TestRunnerWriteCallGraphFile {

    public static void main(String[] args) {
        Assert.assertTrue(new RunnerWriteCallGraphFile().run());
    }
}
