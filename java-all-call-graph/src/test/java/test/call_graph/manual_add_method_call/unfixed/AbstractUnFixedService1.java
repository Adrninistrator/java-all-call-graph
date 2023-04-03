package test.call_graph.manual_add_method_call.unfixed;

import java.util.Collection;

/**
 * @author adrninistrator
 * @date 2022/4/15
 * @description:
 */
public abstract class AbstractUnFixedService1<Req, Rsp extends Collection> {
    public Rsp invoke(Req req, Rsp rsp) {
        return execute(req, rsp);
    }

    protected abstract Rsp execute(Req req, Rsp rsp);
}