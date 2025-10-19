package test.runbycode.el.stringcompare;

import com.adrninistrator.jacg.el.enums.ElConfigEnum;
import com.adrninistrator.javacg2.el.enums.CommonElAllowedVariableEnum;
import org.junit.Test;
import test.callgraph.elexample.callee.TestElExampleCallee1;
import test.callgraph.elexample.caller.TestElExampleCaller1;
import test.runbycode.base.TestElRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2025/9/21
 * @description:
 */
public class TestGenCallGraph4EeElErClassNameOr extends TestElRunByCodeBase {
    @Override
    protected ElConfigEnum chooseElConfigEnum() {
        return ElConfigEnum.ECE_GEN_ALL_CALL_GRAPH_IGNORE_METHOD_CALL;
    }

    @Override
    protected boolean example4StringCompare() {
        return true;
    }

    @Override
    protected String chooseElText() {
        return CommonElAllowedVariableEnum.EAVE_MC_ER_CLASS_NAME.getVariableName() + " == '" + TestElExampleCaller1.class.getName() + "' || " +
                CommonElAllowedVariableEnum.EAVE_MC_ER_CLASS_NAME.getVariableName() + " == '" + TestElExampleCallee1.class.getName() + "'";
    }

    @Override
    protected String chooseTitle() {
        return "判断多个条件使用或";
    }

    @Override
    protected String chooseDesc() {
        return "在生成向上完整方法调用链时，多个条件使用或运算，判断调用类名是否等于指定多个关键字中的任意一个，忽略匹配的类";
    }

    @Test
    public void test() {
        genCalleeGraphAndCheckNoErEe();
    }
}
