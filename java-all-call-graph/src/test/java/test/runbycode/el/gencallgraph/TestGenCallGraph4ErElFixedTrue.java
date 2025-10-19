package test.runbycode.el.gencallgraph;

import com.adrninistrator.jacg.dto.methodcall.MethodCallLineData4Er;
import com.adrninistrator.jacg.el.enums.ElConfigEnum;
import org.junit.Test;
import test.callgraph.elexample.caller.TestElExampleCaller1;
import test.runbycode.base.TestElRunByCodeBase;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2025/9/22
 * @description:
 */
public class TestGenCallGraph4ErElFixedTrue extends TestElRunByCodeBase {
    @Override
    protected ElConfigEnum chooseElConfigEnum() {
        return ElConfigEnum.ECE_GEN_ALL_CALL_GRAPH_IGNORE_METHOD_CALL;
    }

    @Override
    protected String chooseElText() {
        return "true";
    }

    @Override
    protected String chooseTitle() {
        return "生成向下完整方法调用链固定忽略全部";
    }

    @Override
    protected String chooseDesc() {
        return "在生成向下完整方法调用链时，使用固定的表达式，忽略全部方法调用";
    }

    @Test
    public void test() {
        List<MethodCallLineData4Er> methodCallLineData4ErList = genElOneCallerGraph(1);
        checkCallerGraph(methodCallLineData4ErList,
                TestElExampleCaller1.class.getName() + ":test1(" + String.class.getName() + ")");
    }
}