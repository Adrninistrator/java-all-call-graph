package test.runbycode.el.jardiff;

import com.adrninistrator.jacg.el.enums.ElConfigEnum;
import com.adrninistrator.javacg2.el.enums.JavaCG2ElAllowedVariableEnum;
import org.junit.Test;
import test.callgraph.common.TestConstants;
import test.runbycode.base.TestElRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2025/10/1
 * @description:
 */
public class TestGenJarDiffCallerGraph4ClassName extends TestElRunByCodeBase {
    @Override
    protected ElConfigEnum chooseElConfigEnum() {
        return ElConfigEnum.ECE_JAR_DIFF_GEN_ALL_CALL_GRAPH_IGNORE_CALLER;
    }

    @Override
    protected String chooseElText() {
        return JavaCG2ElAllowedVariableEnum.EAVE_PARSE_CLASS_NAME.getVariableName() + " == '" + TestConstants.CLASS_NAME_JAR_DIFF_TEST_CONTROLLER1 + "'";
    }

    @Override
    protected String chooseTitle() {
        return "JarDiff生成向下方法完整调用链判断发生变化的方法类名";
    }

    @Override
    protected String chooseDesc() {
        return "通过JarDiff生成向下方法完整调用链时，判断发生变化的方法类名是否等于指定关键字，忽略匹配的方法";
    }

    @Test
    public void test() {
        genJarDiffCallGraph(false, new String[]{
                TestConstants.CLASS_NAME_JAR_DIFF_TEST_TASK1
        }, new String[]{
                TestConstants.CLASS_NAME_JAR_DIFF_TEST_CONTROLLER1
        });
    }
}