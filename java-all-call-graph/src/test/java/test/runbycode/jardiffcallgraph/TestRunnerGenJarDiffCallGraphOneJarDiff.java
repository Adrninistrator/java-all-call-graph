package test.runbycode.jardiffcallgraph;

import org.junit.Test;
import test.runbycode.jardiffcallgraph.filter.TestModifiedMethodFilter4CalleeGraph1;
import test.runbycode.jardiffcallgraph.filter.TestModifiedMethodFilter4CallerGraph1;

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
    public void testJarDiffCalleeGraphPCD() {
        doTestJarDiffCalleeGraph(false, true);
    }

    @Test
    public void testJarDiffCallerGraph() {
        super.testJarDiffCallerGraph();
    }

    @Test
    public void testJarDiffCalleeGraphFilter() {
        TestModifiedMethodFilter4CalleeGraph1 testModifiedMethodFilter4CalleeGraph1 = new TestModifiedMethodFilter4CalleeGraph1();
        doTestJarDiffCalleeGraph(false, false, testModifiedMethodFilter4CalleeGraph1);
    }

    @Test
    public void testJarDiffCallerGraphFilter() {
        TestModifiedMethodFilter4CallerGraph1 testModifiedMethodFilter4CallerGraph1 = new TestModifiedMethodFilter4CallerGraph1();
        testJarDiffCallerGraph(testModifiedMethodFilter4CallerGraph1);
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
