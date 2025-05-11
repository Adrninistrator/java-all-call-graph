package test.runbycode.jardiffcalleegraph;

import org.junit.Test;

/**
 * @author adrninistrator
 * @date 2025/4/2
 * @description:
 */
public class TestRBCRunnerGenJarDiffCalleeGraphOneJarSame extends TestAbstractRunnerGenJarDiffCalleeGraph {

    /*
        需要先执行 gradlew test_gen_jar 命令
     */
    @Test
    public void testUseLocalConfig() {
        testDiff(true);
    }

    @Test
    public void testNotLocalConfig() {
        testDiff(false);
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
