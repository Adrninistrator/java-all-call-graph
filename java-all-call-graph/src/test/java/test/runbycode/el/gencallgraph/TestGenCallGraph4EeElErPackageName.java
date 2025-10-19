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
public class TestGenCallGraph4EeElErPackageName extends TestElRunByCodeBase {

    public static final String ER_PACKAGE_NAME = "test.callgraph.elexample.caller";
    public static final String ER_PACKAGE_NAME_UPPER = "TEST.CALLGRAPH.ELEXAMPLE.CALLER";

    @Override
    protected ElConfigEnum chooseElConfigEnum() {
        return ElConfigEnum.ECE_GEN_ALL_CALL_GRAPH_IGNORE_METHOD_CALL;
    }

    @Override
    protected String chooseElText() {
        return CommonElAllowedVariableEnum.EAVE_MC_ER_PACKAGE_NAME.getVariableName() + " == '" + ER_PACKAGE_NAME + "'";
    }

    @Override
    protected String chooseTitle() {
        return "生成向上完整方法调用链判断调用类包名";
    }

    @Override
    protected String chooseDesc() {
        return "在生成向上完整方法调用链时，判断调用类包名是否等于指定值，忽略匹配的方法调用";
    }

    @Test
    public void test() {
        genCalleeGraphAndCheckNoEr();
    }
}
