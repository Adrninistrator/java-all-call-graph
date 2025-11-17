package test.runbycode.el.gencallgraph;

import com.adrninistrator.jacg.dto.methodcall.MethodCallLineData4Ee;
import com.adrninistrator.jacg.el.enums.ElConfigEnum;
import com.adrninistrator.javacg2.common.enums.JavaCG2CallTypeEnum;
import com.adrninistrator.javacg2.el.enums.CommonElAllowedVariableEnum;
import org.junit.Test;
import test.callgraph.elexample.callee.TestElExampleCallee1;
import test.runbycode.base.TestElRunByCodeBase;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2025/9/12
 * @description:
 */
public class TestGenCallGraph4EeElMethodCallType extends TestElRunByCodeBase {
    @Override
    protected ElConfigEnum chooseElConfigEnum() {
        return ElConfigEnum.ECE_GEN_ALL_CALL_GRAPH_IGNORE_METHOD_CALL;
    }

    @Override
    protected String chooseElText() {
        return CommonElAllowedVariableEnum.EAVE_METHOD_CALL_TYPE.getVariableName() + " == '" + JavaCG2CallTypeEnum.CTE_RAW_INVOKE_STATIC.getType() + "'";
    }

    @Override
    protected String chooseTitle() {
        return "生成向上方法完整调用链判断调用类型";
    }

    @Override
    protected String chooseDesc() {
        return "在生成向上方法完整调用链时，判断调用类型是否等于指定值，忽略匹配的方法调用";
    }

    @Test
    public void test() {
        List<MethodCallLineData4Ee> methodCallLineData4EeList = genElOneCalleeGraph();
        checkCalleeGraph(methodCallLineData4EeList,
                TestElExampleCallee1.class.getName() + ":testC()");
    }
}
