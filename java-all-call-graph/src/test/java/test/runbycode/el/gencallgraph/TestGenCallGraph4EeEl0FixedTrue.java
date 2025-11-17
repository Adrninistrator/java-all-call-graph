package test.runbycode.el.gencallgraph;

import com.adrninistrator.jacg.el.enums.ElConfigEnum;
import org.junit.Test;
import test.runbycode.base.TestElRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2025/9/18
 * @description:
 */
public class TestGenCallGraph4EeEl0FixedTrue extends TestElRunByCodeBase {
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
        return "生成向上方法完整调用链固定忽略全部";
    }

    @Override
    protected String chooseDesc() {
        return "在生成向上方法完整调用链时，使用固定的表达式，忽略全部方法调用";
    }

    @Test
    public void test() {
        commonWriteDbForce();

        genCalleeGraphAndCheckNoErEe();
    }
}