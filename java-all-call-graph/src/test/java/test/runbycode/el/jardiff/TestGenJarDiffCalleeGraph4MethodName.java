package test.runbycode.el.jardiff;

import com.adrninistrator.jacg.el.enums.ElConfigEnum;
import com.adrninistrator.javacg2.el.enums.JavaCG2ElAllowedVariableEnum;
import org.junit.Test;
import test.callgraph.common.TestConstants;
import test.runbycode.base.TestElRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2025/9/30
 * @description:
 */
public class TestGenJarDiffCalleeGraph4MethodName extends TestElRunByCodeBase {
    @Override
    protected ElConfigEnum chooseElConfigEnum() {
        return ElConfigEnum.ECE_JAR_DIFF_GEN_ALL_CALL_GRAPH_IGNORE_CALLEE;
    }

    @Override
    protected String chooseElText() {
        return JavaCG2ElAllowedVariableEnum.EAVE_PARSE_METHOD_NAME.getVariableName() + " == 'get1'";
    }

    @Override
    protected String chooseTitle() {
        return "JarDiff生成向上方法完整调用链判断发生变化的方法名";
    }

    @Override
    protected String chooseDesc() {
        return "通过JarDiff生成向上方法完整调用链时，判断发生变化的方法名是否等于指定关键字，忽略匹配的方法";
    }

    @Test
    public void test() {
        genJarDiffCallGraph(true, new String[]{
                TestConstants.CLASS_NAME_JAR_DIFF_TEST_TASK1,
                TestConstants.CLASS_NAME_JAR_DIFF_TEST_CONTROLLER1 + ":get2()"
        }, new String[]{
                TestConstants.CLASS_NAME_JAR_DIFF_TEST_CONTROLLER1 + ":get1()"
        });
    }
}