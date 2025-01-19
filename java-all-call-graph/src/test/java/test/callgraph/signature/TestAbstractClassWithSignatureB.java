package test.callgraph.signature;

/**
 * @author adrninistrator
 * @date 2025/1/5
 * @description:
 */
public abstract class TestAbstractClassWithSignatureB<N, T1, T2, M> implements TestInterfaceWithSignature1<T1, T2> {

    public abstract void testB1(N n, T1 t1, T2 t2, M m);
}
