package test.runbycode.el.gencallgraph;

import com.adrninistrator.jacg.dto.methodcall.MethodCallLineData4Ee;
import com.adrninistrator.jacg.el.enums.ElAllowedVariableEnum;
import com.adrninistrator.jacg.el.enums.ElConfigEnum;
import org.junit.Test;
import test.callgraph.elexample.caller.TestElExampleCaller2;
import test.runbycode.base.TestElRunByCodeBase;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2025/9/26
 * @description:
 */
public class TestGenCallGraph4EeElFlagsExcludeAll extends TestElRunByCodeBase {
    @Override
    protected ElConfigEnum chooseElConfigEnum() {
        return ElConfigEnum.ECE_GEN_ALL_CALL_GRAPH_IGNORE_METHOD_CALL;
    }

    @Override
    protected String chooseElText() {
        return "count(" + ElAllowedVariableEnum.EAVE_MC_FLAGS_ENUM.getVariableName() + ") > 0";
    }

    @Override
    protected String chooseTitle() {
        return "生成向上完整方法调用链忽略所有调用标志";
    }

    @Override
    protected String chooseDesc() {
        return "在生成向上完整方法调用链时，忽略所有存在调用标志的方法调用";
    }

    @Test
    public void test() {
        List<MethodCallLineData4Ee> methodCallLineData4EeList = genElOneCalleeGraph(1, METHOD_EXAMPLE_CALLER_2_TEST3);
        checkCalleeGraph(methodCallLineData4EeList,
                TestElExampleCaller2.class.getName() + ":test3()");
    }
}