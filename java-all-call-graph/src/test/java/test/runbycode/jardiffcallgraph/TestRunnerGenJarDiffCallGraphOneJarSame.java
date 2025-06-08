package test.runbycode.jardiffcallgraph;

import org.junit.Test;

/**
 * @author adrninistrator
 * @date 2025/4/2
 * @description: 比较一个jar文件，新版本与旧版本相同
 */
public class TestRunnerGenJarDiffCallGraphOneJarSame extends TestAbstractRunnerGenJarDiffCallGraph {

    /*
        需要先执行以下命令
        gradlew test_gen_jar
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
        return "jar_diff_jar_same";
    }

    @Override
    protected String chooseRootJarDir() {
        return "build/";
    }

    @Override
    protected String chooseDirNameOld() {
        return "test_jar_1";
    }

    @Override
    protected String chooseDirNameNew() {
        return "test_jar_2";
    }

    @Override
    protected boolean isResultEmpty() {
        return true;
    }
}
