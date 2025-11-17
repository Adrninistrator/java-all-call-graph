package test.runbycode.el.gencallgraph;

import com.adrninistrator.jacg.el.enums.ElConfigEnum;
import com.adrninistrator.javacg2.el.enums.CommonElAllowedVariableEnum;
import org.junit.Test;
import test.callgraph.elexample.caller.TestElExampleCaller1;
import test.runbycode.base.TestElRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2025/9/22
 * @description:
 */
public class TestGenCallGraph4EeElErSimpleClassName extends TestElRunByCodeBase {

    @Override
    protected ElConfigEnum chooseElConfigEnum() {
        return ElConfigEnum.ECE_GEN_ALL_CALL_GRAPH_IGNORE_METHOD_CALL;
    }

    @Override
    protected String chooseElText() {
        return CommonElAllowedVariableEnum.EAVE_MC_ER_SIMPLE_CLASS_NAME.getVariableName() + " == '" + TestElExampleCaller1.class.getSimpleName() + "'";
    }

    @Override
    protected String chooseTitle() {
        return "生成向上方法完整调用链判断调用类简单类名";
    }

    @Override
    protected String chooseDesc() {
        return "在生成向上方法完整调用链时，判断调用类简单类名是否等于指定值，忽略匹配的方法调用";
    }

    @Test
    public void test() {
        genCalleeGraphAndCheckNoEr();
    }
}
