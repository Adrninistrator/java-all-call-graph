package test.call_graph.manual_add_method_call.fixed;

import test.call_graph.other_jar.AbstractFixedOtherJarService1;

/**
 * @author adrninistrator
 * @date 2023/3/24
 * @description:
 */
public class FixedOtherJarService1a extends AbstractFixedOtherJarService1 {
    @Override
    protected String execute() {
        return System.getProperty("aaa");
    }
}
