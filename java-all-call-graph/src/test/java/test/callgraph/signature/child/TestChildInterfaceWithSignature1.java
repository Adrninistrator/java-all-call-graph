package test.callgraph.signature.child;

import test.callgraph.signature.TestInterfaceWithSignature1;

/**
 * @author adrninistrator
 * @date 2025/1/5
 * @description:
 */
public interface TestChildInterfaceWithSignature1<N, M, T1, T2> extends TestInterfaceWithSignature1<T1, T2> {

    void testC1(N n, M m, T1 t1, T2 t2);
}
