package test.callgraph.manualaddmethodcall.unfixed;

import test.callgraph.otherjar.AbstractUnFixedOtherJarService1;

import java.util.LinkedList;

/**
 * @author adrninistrator
 * @date 2023/3/25
 * @description:
 */
public class UnfixedOtherJarService1 extends AbstractUnFixedOtherJarService1<Long, LinkedList<String>> {
    @Override
    protected LinkedList<String> execute(Long l, LinkedList<String> list) {
        System.getProperty(null);
        return null;
    }
}