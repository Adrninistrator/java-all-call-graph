package test.runbycode.jardiffcallgraph;

import org.junit.Test;

/**
 * @author adrninistrator
 * @date 2024/4/20
 * @description: 比较一个jar文件，新版本与旧版本不同
 */
public class TestRunnerGenJarDiffCallGraphOneJarDiff extends TestAbstractRunnerGenJarDiffCallGraph {

    /*
        需要先执行以下命令
        gradlew test_gen_diff_jar -Pexample_flag=1
        gradlew test_gen_diff_jar -Pexample_flag=2
     */
    @Test
    public void testJarDiffCalleeGraph() {
        super.testJarDiffCalleeGraph();
    }

    @Test
    public void testJarDiffCallerGraph() {
        super.testJarDiffCallerGraph();
    }

    @Test
    public void testJarDiffCalleeGraphFilter() {
        doTestJarDiffCalleeGraph(false, false, false);
    }

    @Test
    public void testJarDiffCallerGraphFilter() {
        testJarDiffCallerGraph();
    }

    @Override
    protected String chooseAppName() {
        return "jar_diff_jar_diff";
    }

    @Override
    protected String chooseRootJarDir() {
        return "build/";
    }

    @Override
    protected String chooseDirNameOld() {
        return "jar-diff-version-1";
    }

    @Override
    protected String chooseDirNameNew() {
        return "jar-diff-version-2";
    }

    @Override
    protected boolean isResultEmpty() {
        return false;
    }
}
