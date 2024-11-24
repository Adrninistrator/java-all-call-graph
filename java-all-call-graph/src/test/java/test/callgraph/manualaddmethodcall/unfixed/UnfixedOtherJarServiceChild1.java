package test.callgraph.manualaddmethodcall.unfixed;

import test.callgraph.methodargument.TestArgument1;

import java.util.LinkedList;

/**
 * @author adrninistrator
 * @date 2023/3/25
 * @description:
 */
public class UnfixedOtherJarServiceChild1 extends AbstractUnFixedOtherJarServiceChild1<TestArgument1, LinkedList> {
    @Override
    protected java.util.LinkedList execute(test.callgraph.methodargument.TestArgument1 t, java.util.LinkedList list) {
        System.getProperty(null);
        return null;
    }
}