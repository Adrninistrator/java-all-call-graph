package test.runbycode.jardiffcallgraph;

import org.junit.Test;

/**
 * @author adrninistrator
 * @date 2025/9/30
 * @description:
 */
public class TestRunnerGenJarDiffCallGraph0 extends TestAbstractRunnerGenJarDiffCallGraph {

    @Test
    public void testCalleeGraph() {
        doTestJarDiffCalleeGraph(false, false, false);
    }

    @Test
    public void testCallerGraph() {
        testJarDiffCallerGraph();
    }

    @Override
    protected String chooseAppName() {
        return TestRunnerGenJarDiffCallGraphDiffSame.APP_NAME;
    }

    @Override
    protected String chooseRootJarDir() {
        return TestRunnerGenJarDiffCallGraphDiffSame.ROOT_JAR_DIR;
    }

    @Override
    protected String chooseDirNameOld() {
        return TestRunnerGenJarDiffCallGraphDiffSame.DIR_NAME_OLD;
    }

    @Override
    protected String chooseDirNameNew() {
        return TestRunnerGenJarDiffCallGraphDiffSame.DIR_NAME_NEW;
    }

    @Override
    protected boolean isResultEmpty() {
        return false;
    }
}
