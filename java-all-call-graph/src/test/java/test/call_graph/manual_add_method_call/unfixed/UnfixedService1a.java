package test.call_graph.manual_add_method_call.unfixed;

import java.util.LinkedList;

/**
 * @author adrninistrator
 * @date 2022/4/15
 * @description:
 */
public class UnfixedService1a extends AbstractUnFixedService1<Long, LinkedList<String>> {
    @Override
    protected LinkedList<String> execute(Long l, LinkedList<String> list) {
        System.getProperty(null);
        return null;
    }
}
