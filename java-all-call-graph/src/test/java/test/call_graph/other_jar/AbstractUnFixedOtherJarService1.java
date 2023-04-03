package test.call_graph.other_jar;

import java.util.Collection;

/**
 * @author adrninistrator
 * @date 2023/3/25
 * @description:
 */
public abstract class AbstractUnFixedOtherJarService1<Req, Rsp extends Collection> {
    public Rsp invoke(Req req, Rsp rsp) {
        return execute(req, rsp);
    }

    protected abstract Rsp execute(Req req, Rsp rsp);
}