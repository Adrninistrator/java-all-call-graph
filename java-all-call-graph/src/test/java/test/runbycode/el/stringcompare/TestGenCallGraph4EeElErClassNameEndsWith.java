package test.runbycode.el.stringcompare;

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
public class TestGenCallGraph4EeElErClassNameEndsWith extends TestElRunByCodeBase {
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
        return "string.endsWith(" + CommonElAllowedVariableEnum.EAVE_MC_ER_CLASS_NAME.getVariableName() + ", '" + TestElExampleCaller1.class.getName() + "')";
    }

    @Override
    protected String chooseTitle() {
        return "判断以关键字结尾";
    }

    @Override
    protected String chooseDesc() {
        return "在生成向上方法完整调用链时，判断调用类名是否以指定关键字结尾，忽略匹配的方法调用";
    }

    @Test
    public void test() {
        genCalleeGraphAndCheckNoEr();
    }
}
