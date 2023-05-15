package test.run_by_code.handler.extends_impl;

import com.adrninistrator.jacg.handler.extends_impl.JACGExtendsImplHandler;
import org.junit.Test;
import test.call_graph.interfaces.interfaces.InterfaceSuper1;
import test.run_by_code.base.TestRunByCodeBase;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/1/5
 * @description:
 */
public class TestQueryExtendsImpl extends TestRunByCodeBase {

    @Test
    public void testGetChildClassList() {
        try (JACGExtendsImplHandler jacgExtendsImplHandler = new JACGExtendsImplHandler(configureWrapper)) {
            doTestGetChildClassList(jacgExtendsImplHandler, InterfaceSuper1.class.getName());
        }
    }

    private void doTestGetChildClassList(JACGExtendsImplHandler jacgExtendsImplHandler, String className) {
        List<String> list = jacgExtendsImplHandler.queryChildClassListByFull(className, true, true, true, true);
        printListContent(list, className, "子接口+子类");

        list = jacgExtendsImplHandler.queryChildClassListByFull(className, true, false, false, false);
        printListContent(list, className, "接口");

        list = jacgExtendsImplHandler.queryChildClassListByFull(className, false, true, true, true);
        printListContent(list, className, "类");

        list = jacgExtendsImplHandler.queryChildClassListByFull(className, false, true, true, false);
        printListContent(list, className, "抽象类 {");

        list = jacgExtendsImplHandler.queryChildClassListByFull(className, false, true, false, true);
        printListContent(list, className, "非抽象类");
    }
}
