package test.runbycode.el.jardiff;

import com.adrninistrator.jacg.el.enums.ElConfigEnum;
import org.junit.Test;
import test.diffjar.controller.TestController1;
import test.diffjar.task.TestTask1;
import test.runbycode.base.TestElRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2025/9/30
 * @description:
 */
public class TestGenJarDiffCalleeGraphFixedFalse extends TestElRunByCodeBase {
    @Override
    protected ElConfigEnum chooseElConfigEnum() {
        return ElConfigEnum.ECE_JAR_DIFF_GEN_ALL_CALL_GRAPH_IGNORE_CALLEE;
    }

    @Override
    protected String chooseElText() {
        return "false";
    }

    @Override
    protected String chooseTitle() {
        return "JarDiff生成向上完整方法调用链固定全部不忽略";
    }

    @Override
    protected String chooseDesc() {
        return "通过JarDiff生成向上完整方法调用链时，使用固定的表达式，方法全部不忽略";
    }

    @Test
    public void test() {
        genJarDiffCallGraph(true, new String[]{
                TestController1.class.getName(),
                TestTask1.class.getName()
        }, new String[]{});
    }
}