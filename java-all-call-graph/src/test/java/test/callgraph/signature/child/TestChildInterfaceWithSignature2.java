package test.callgraph.signature.child;

/**
 * @author adrninistrator
 * @date 2025/1/6
 * @description:
 */
public interface TestChildInterfaceWithSignature2<D, C, B, A> extends TestChildInterfaceWithSignature1<A, B, C, D> {

    void fC2(A a, B b, C c, D d);
}
