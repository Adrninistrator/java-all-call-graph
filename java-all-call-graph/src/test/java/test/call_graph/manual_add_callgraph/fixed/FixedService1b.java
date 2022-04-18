package test.call_graph.manual_add_callgraph.fixed;

/**
 * @author adrninistrator
 * @date 2022/4/15
 * @description:
 */
public class FixedService1b extends AbstractFixedService1 {
    @Override
    protected String execute() {
        return System.getProperty("");
    }
}
