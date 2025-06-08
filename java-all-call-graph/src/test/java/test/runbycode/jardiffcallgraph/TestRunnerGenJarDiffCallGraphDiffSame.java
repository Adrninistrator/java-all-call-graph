package test.runbycode.jardiffcallgraph;

import org.junit.Test;

/**
 * @author adrninistrator
 * @date 2025/4/2
 * @description: 比较的新旧版本jar文件有相同的，也有不同的
 */
public class TestRunnerGenJarDiffCallGraphDiffSame extends TestAbstractRunnerGenJarDiffCallGraph {

    /*
        需要先执行以下命令
        gradlew test_gen_diff_jar_with_same -Pexample_flag=1
        gradlew test_gen_diff_jar_with_same -Pexample_flag=2
     */
    @Test
    public void testJarDiffCalleeGraph() {
        super.testJarDiffCalleeGraph();
    }

    @Test
    public void testJarDiffCallerGraph() {
        super.testJarDiffCallerGraph();
    }

    @Override
    protected String chooseAppName() {
        return "jar_diff_diff_same";
    }

    @Override
    protected String chooseRootJarDir() {
        return "build/";
    }

    @Override
    protected String chooseDirNameOld() {
        return "jar-diff-with-same-1";
    }

    @Override
    protected String chooseDirNameNew() {
        return "jar-diff-with-same-2";
    }

    @Override
    protected boolean isResultEmpty() {
        return false;
    }
}
