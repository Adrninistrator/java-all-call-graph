package test.callgraph.signature;

/**
 * @author adrninistrator
 * @date 2022/12/7
 * @description:
 */
public abstract class TestAbstractClassWithSignature<T1, T2> implements TestInterfaceWithSignature<T1, T2> {

    public void superMethod1(T1 t1) {
        System.out.println(t1.getClass().getName());
    }

    public void superMethod2(T2 t2) {
        System.out.println(t2.getClass().getName());
    }
}
