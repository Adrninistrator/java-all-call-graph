package test.call_graph.manual_add_method_call.unfixed;

import test.call_graph.argument.TestArgument1;

import java.util.LinkedList;

/**
 * @author adrninistrator
 * @date 2023/3/25
 * @description:
 */
public class UnfixedOtherJarServiceChild1 extends AbstractUnFixedOtherJarServiceChild1<TestArgument1, LinkedList> {
    @Override
    protected java.util.LinkedList execute(test.call_graph.argument.TestArgument1 t, java.util.LinkedList list) {
        System.getProperty(null);
        return null;
    }
}