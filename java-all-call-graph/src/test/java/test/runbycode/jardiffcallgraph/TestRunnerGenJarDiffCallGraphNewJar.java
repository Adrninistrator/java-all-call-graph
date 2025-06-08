package test.runbycode.jardiffcallgraph;

import org.junit.Test;

/**
 * @author adrninistrator
 * @date 2025/5/18
 * @description: 比较的新版本jar文件在旧版本中不存在
 */
public class TestRunnerGenJarDiffCallGraphNewJar extends TestAbstractRunnerGenJarDiffCallGraph {

    /*
        需要先执行以下命令
        gradlew test_gen_jar
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

    @Override
    protected String chooseAppName() {
        return "jar_diff_new_jar";
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
        return "jar-diff-version-2";
    }

    @Override
    protected boolean isResultEmpty() {
        return false;
    }
}
