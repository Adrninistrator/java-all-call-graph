package test.callgraph.interfaces;

import test.callgraph.interfaces.interfaces.extend.BaseMapper;

/**
 * @author adrninistrator
 * @date 2024/7/14
 * @description:
 */
public abstract class AbstractMapper<E, N, T2> implements BaseMapper<T2> {

    public abstract E getE();

    public abstract N getN();
}
