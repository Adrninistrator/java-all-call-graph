package test.call_graph.manual_add_method_call.unfixed;

import test.call_graph.other_jar.AbstractUnFixedOtherJarService1;

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