package test.callgraph.interfacesgeneric.interfaces;

import test.callgraph.extendcomplex.AbstractSuperClassA;

/**
 * @author adrninistrator
 * @date 2023/7/9
 * @description:
 */
public interface GenericInterfaceSuper1<T1 extends AbstractSuperClassA, T2 extends AbstractSuperClassA> {
    void test(T1 t1, T2 t2, String str, int i);
}
