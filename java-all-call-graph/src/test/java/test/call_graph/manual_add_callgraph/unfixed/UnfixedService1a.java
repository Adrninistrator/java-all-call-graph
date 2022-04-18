package test.call_graph.manual_add_callgraph.unfixed;

/**
 * @author adrninistrator
 * @date 2022/4/15
 * @description:
 */
public class UnfixedService1a extends AbstractUnFixedService1<String, String> {
    @Override
    protected String execute(String s) {
        return System.getProperty(s);
    }
}
