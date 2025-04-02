package test.runbycode.jardiffcalleegraph;

import org.junit.Test;

/**
 * @author adrninistrator
 * @date 2024/4/20
 * @description:
 */
public class TestRBCRunnerGenJarDiffCalleeGraphOneJarDiff extends TestAbstractRunnerGenJarDiffCalleeGraph {

    /*
        需要先执行以下命令
        gradlew test_gen_diff_tar_gz -Pexample_flag=1
        gradlew test_gen_diff_tar_gz -Pexample_flag=2
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
}
