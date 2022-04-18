package test.call_graph.manual_add_callgraph.fixed;

/**
 * @author adrninistrator
 * @date 2022/4/15
 * @description:
 */
public abstract class AbstractFixedService1 {

    public String invoke() {
        return execute();
    }

    protected abstract String execute();
}
