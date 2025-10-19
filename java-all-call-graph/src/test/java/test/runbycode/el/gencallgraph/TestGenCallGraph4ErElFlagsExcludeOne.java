package test.runbycode.el.gencallgraph;

import com.adrninistrator.jacg.common.enums.MethodCallFlagsEnum;
import com.adrninistrator.jacg.dto.methodcall.MethodCallLineData4Er;
import com.adrninistrator.jacg.el.enums.ElAllowedVariableEnum;
import com.adrninistrator.jacg.el.enums.ElConfigEnum;
import org.junit.Test;
import test.callgraph.elexample.caller.TestElExampleCaller2;
import test.runbycode.base.TestElRunByCodeBase;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2025/9/27
 * @description:
 */
public class TestGenCallGraph4ErElFlagsExcludeOne extends TestElRunByCodeBase {
    @Override
    protected ElConfigEnum chooseElConfigEnum() {
        return ElConfigEnum.ECE_GEN_ALL_CALL_GRAPH_IGNORE_METHOD_CALL;
    }

    @Override
    protected String chooseElText() {
        return "include(" + ElAllowedVariableEnum.EAVE_MC_FLAGS_ENUM.getVariableName() + ", '" + MethodCallFlagsEnum.MCFE_EE_DTO_GET_SET_METHOD.name() + "')";
    }

    @Override
    protected String chooseTitle() {
        return "生成向下完整方法调用链判断调用标志";
    }

    @Override
    protected String chooseDesc() {
        return "在生成向下完整方法调用链时，判断调用标志是否包括指定的标志，忽略匹配的方法调用";
    }

    @Test
    public void test() {
        List<MethodCallLineData4Er> methodCallLineData4ErList = genElOneCallerGraph(METHOD_EXAMPLE_CALLER_2_TEST1);
        checkCallerGraph(methodCallLineData4ErList,
                TestElExampleCaller2.class.getName() + ":test1()",
                TestElExampleCaller2.class.getName() + ":test2()",
                TestElExampleCaller2.class.getName() + ":test3()");
    }
}