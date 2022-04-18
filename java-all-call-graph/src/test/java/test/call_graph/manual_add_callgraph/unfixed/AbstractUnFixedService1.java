package test.call_graph.manual_add_callgraph.unfixed;

/**
 * @author adrninistrator
 * @date 2022/4/15
 * @description:
 */
public abstract class AbstractUnFixedService1<Req, Rsp> {
    public Rsp invoke(Req req) {
        return execute(req);
    }

    protected abstract Rsp execute(Req req);
}