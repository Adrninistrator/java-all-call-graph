package test.runbycode.jardiffcalleegraph;

import org.junit.Test;

/**
 * @author adrninistrator
 * @date 2025/4/2
 * @description:
 */
public class TestRBCRunnerGenJarDiffCalleeGraphDiffSame extends TestAbstractRunnerGenJarDiffCalleeGraph {

    /*
        需要先执行以下命令
        gradlew test_gen_diff_jar_with_same -Pexample_flag=1
        gradlew test_gen_diff_jar_with_same -Pexample_flag=2
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
        return "jar_diff_diff_same";
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
}
