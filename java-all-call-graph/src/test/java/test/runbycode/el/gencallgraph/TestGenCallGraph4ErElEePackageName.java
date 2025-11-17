package test.runbycode.el.gencallgraph;

import com.adrninistrator.jacg.el.enums.ElConfigEnum;
import com.adrninistrator.javacg2.el.enums.CommonElAllowedVariableEnum;
import org.junit.Test;
import test.runbycode.base.TestElRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2025/9/22
 * @description:
 */
public class TestGenCallGraph4ErElEePackageName extends TestElRunByCodeBase {

    public static final String ER_PACKAGE_NAME = "test.callgraph.elexample.callee";

    @Override
    protected ElConfigEnum chooseElConfigEnum() {
        return ElConfigEnum.ECE_GEN_ALL_CALL_GRAPH_IGNORE_METHOD_CALL;
    }

    @Override
    protected String chooseElText() {
        return CommonElAllowedVariableEnum.EAVE_MC_EE_PACKAGE_NAME.getVariableName() + " == '" + ER_PACKAGE_NAME + "'";
    }

    @Override
    protected String chooseTitle() {
        return "生成向下方法完整调用链判断被调用类包名";
    }

    @Override
    protected String chooseDesc() {
        return "在生成向下方法完整调用链时，判断被调用类包名是否等于指定值，忽略匹配的方法调用";
    }

    @Test
    public void test() {
        genCallerGraphAndCheckNoEe();
    }
}