package test.runbycode.el.gencallgraph;

import com.adrninistrator.jacg.dto.methodcall.MethodCallLineData4Ee;
import com.adrninistrator.jacg.el.enums.ElConfigEnum;
import org.junit.Test;
import test.callgraph.elexample.callee.TestElExampleCallee1;
import test.callgraph.elexample.caller.TestElExampleCaller1;
import test.runbycode.base.TestElRunByCodeBase;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2025/9/18
 * @description:
 */
public class TestGenCallGraph4EeElFixedFalse extends TestElRunByCodeBase {
    @Override
    protected ElConfigEnum chooseElConfigEnum() {
        return ElConfigEnum.ECE_GEN_ALL_CALL_GRAPH_IGNORE_METHOD_CALL;
    }

    @Override
    protected String chooseElText() {
        return "false";
    }

    @Override
    protected String chooseTitle() {
        return "生成向上方法完整调用链固定全部不忽略";
    }

    @Override
    protected String chooseDesc() {
        return "在生成向上方法完整调用链时，使用固定的表达式，方法调用全部不忽略";
    }

    @Test
    public void test() {
        List<MethodCallLineData4Ee> methodCallLineData4EeList = genElOneCalleeGraph();
        checkCalleeGraph(methodCallLineData4EeList,
                TestElExampleCallee1.class.getName() + ":testC()",
                TestElExampleCallee1.class.getName() + ":testB()",
                TestElExampleCallee1.class.getName() + ":testA()",
                TestElExampleCaller1.class.getName() + ":test1(" + String.class.getName() + ")");
    }
}