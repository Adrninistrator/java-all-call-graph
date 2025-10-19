package test.runbycode.jardiffcallgraph;

import com.adrninistrator.jacg.el.enums.ElConfigEnum;
import com.adrninistrator.javacg2.el.enums.JavaCG2ElAllowedVariableEnum;
import org.junit.Test;

/**
 * @author adrninistrator
 * @date 2025/4/2
 * @description: 比较的新旧版本jar文件有相同的，也有不同的
 */
public class TestRunnerGenJarDiffCallGraphDiffSame extends TestAbstractRunnerGenJarDiffCallGraph {

    public static final String APP_NAME = "jar_diff_diff_same";
    public static final String ROOT_JAR_DIR = "build/";
    public static final String DIR_NAME_OLD = "jar-diff-with-same-1";
    public static final String DIR_NAME_NEW = "jar-diff-with-same-2";

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

    @Test
    public void testJarDiffCalleeGraphChooseMethod() {
        // 指定发生变化的方法中哪些需要处理
        configureWrapper.setElConfigText(ElConfigEnum.ECE_JAR_DIFF_GEN_ALL_CALL_GRAPH_IGNORE_CALLEE,
                JavaCG2ElAllowedVariableEnum.EAVE_PARSE_PACKAGE_NAME.getVariableName() + " == 'test.diffjar.service' || " +
                        "string.startsWith(" + JavaCG2ElAllowedVariableEnum.EAVE_PARSE_PACKAGE_NAME.getVariableName() + ", 'test.diffjar.service.')");
        super.testJarDiffCalleeGraph();
    }

    @Test
    public void testJarDiffCallerGraphChooseMethod() {
        // 指定发生变化的方法中哪些需要处理
        configureWrapper.setElConfigText(ElConfigEnum.ECE_JAR_DIFF_GEN_ALL_CALL_GRAPH_IGNORE_CALLER,
                JavaCG2ElAllowedVariableEnum.EAVE_PARSE_PACKAGE_NAME.getVariableName() + " == 'test.diffjar.controller' || " +
                        "string.startsWith(" + JavaCG2ElAllowedVariableEnum.EAVE_PARSE_PACKAGE_NAME.getVariableName() + ", 'test.diffjar.controller.')");
        super.testJarDiffCallerGraph();
    }

    @Override
    protected String chooseAppName() {
        return APP_NAME;
    }

    @Override
    protected String chooseRootJarDir() {
        return ROOT_JAR_DIR;
    }

    @Override
    protected String chooseDirNameOld() {
        return DIR_NAME_OLD;
    }

    @Override
    protected String chooseDirNameNew() {
        return DIR_NAME_NEW;
    }

    @Override
    protected boolean isResultEmpty() {
        return false;
    }
}
