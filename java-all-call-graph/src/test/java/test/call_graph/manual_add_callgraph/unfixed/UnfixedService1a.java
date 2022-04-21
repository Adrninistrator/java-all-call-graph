package test.call_graph.manual_add_callgraph.unfixed;

import java.util.LinkedList;

/**
 * @author adrninistrator
 * @date 2022/4/15
 * @description:
 */
public class UnfixedService1a extends AbstractUnFixedService1<Long, LinkedList> {
    @Override
    protected LinkedList execute(Long l, LinkedList list) {
        System.getProperty(null);
        return null;
    }
}
